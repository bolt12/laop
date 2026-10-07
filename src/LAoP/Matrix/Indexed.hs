{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Indexed
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrices indexed by your own types: start here. A @'Matrix' e a b@ is an arrow
from @a@ to @b@, with a column for every value of @a@ and a row for every value
of @b@, so @'comp' f g@ applies @g@ and then @f@, as functions compose. Any
type with a 'MatIndex' instance can index a dimension: an enumeration with a
'Generic' instance needs only an empty instance, and @()@, 'Bool', 'Either',
pairs and the types of "LAoP.Index" have one.

The functions here wrap those of "LAoP.Matrix.Internal", where the matrix type
and the algorithms are.
-}
module LAoP.Matrix.Indexed (
  -- * Matrix type
  Matrix (..),

  -- * Index types
  MatIndex (..),
  GDimOf,
  GMatIndex,
  cardinality,
  One,

  -- * Dimension kind and witnesses
  I.Dim (..),
  I.SDim,
  I.KnownDim (..),

  -- * Primitives
  one,
  join,
  fork,

  -- * Block decomposition
  splitJoin,
  splitFork,

  -- * Construction and conversion
  fromLists,
  toLists,
  toList,
  scalar,
  matrixBuilder',
  matrixBuilder,
  fromF,
  col,
  row,
  zeros,
  ones,
  bang,
  point,
  constant,

  -- * Composition and transposition
  iden,
  comp,
  parComp,
  parCompWith,
  tr,

  -- * Element-wise operations
  (.+.),
  (.-.),
  (.*.),
  zipWithM,
  emap,

  -- * Scalar operations
  (.|),
  (./),

  -- * Biproduct
  (===),
  (|||),
  p1,
  p2,
  i1,
  i2,

  -- * Bifunctors
  (-|-),
  (><),
  bimapM,

  -- * Pairing
  fstM,
  sndM,
  kr,
  unitM,
  multM,

  -- * Selective and conditional
  selectM,
  select,
  branch,
  cond,

  -- * Matrix "abiding"
  abideJF,
  abideFJ,

  -- * Relations
  I.Boolean (..),
  toRel,

  -- * Dimensions
  columns,
  rows,

  -- * Pretty printing
  pretty,
  prettyPrint,
) where

import           Control.DeepSeq
import           Data.Array               (Array, listArray, (!))
import           Data.Bits                (bit, testBit, (.|.))
import           Data.Coerce              (coerce)
import           Data.Kind                (Type)
import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic (..), K1, M1, U1 (..), V1,
                                           (:*:), (:+:) (..))
import qualified GHC.Generics             as G
import           GHC.Stack                (HasCallStack)
import           GHC.TypeError            (Unsatisfiable)
import           GHC.TypeLits             (ErrorMessage (..), KnownNat,
                                           TypeError, natVal, type (+),
                                           type (-), type (<=))
import           LAoP.Category
import           LAoP.Index
import qualified LAoP.Matrix.Internal     as I
import           LAoP.Matrix.Internal.Dim (natSize, unsafeSFromNat)
import           Prelude                  hiding (id, (.))

-- Generic derivation support

{- | Computes the @Dim@ for a type from its @GHC.Generics@ representation.
It is the default 'DimOf', so an empty @instance MatIndex T@ is enough for an
enumeration @T@ with a 'Generic' instance.
-}
type GDimOf :: Type -> I.Dim
type family GDimOf a where
  GDimOf a = GRepDimOf (Rep a)

type NotAnEnumeration =
  'Text "GDimOf: only enumerations (constructors without fields) have a generic MatIndex."
    ':$$: 'Text "Write the MatIndex instance by hand, or use Either, (,) or Ranged indices."

type GRepDimOf :: (Type -> Type) -> I.Dim
type family GRepDimOf rep where
  GRepDimOf (M1 _i _c f) = GRepDimOf f
  GRepDimOf U1            = I.U
  GRepDimOf (f :+: g)     = GRepDimOf f I.:+: GRepDimOf g
  GRepDimOf (K1 _i _c)    = TypeError NotAnEnumeration
  GRepDimOf (_f :*: _g)   = TypeError NotAnEnumeration
  GRepDimOf V1            = TypeError ('Text "GDimOf: an empty type has no MatIndex.")

-- | Generic implementation of 'MatIndex' for enumerations.
class GMatIndex (rep :: Type -> Type) where
  gDim     :: I.SDim (GRepDimOf rep)
  gToOrd   :: rep p -> Int
  gFromOrd :: Int -> rep p

instance GMatIndex f => GMatIndex (M1 i c f) where
  gDim = gDim @f
  gToOrd (G.M1 x) = gToOrd x
  gFromOrd i = G.M1 (gFromOrd i)

instance GMatIndex U1 where
  gDim = I.SU
  gToOrd U1 = 0
  gFromOrd _ = U1

-- Constructors with fields, products and empty types have no generic index;
-- these instances turn the missing-instance error into an explanation.
instance (Unsatisfiable NotAnEnumeration) => GMatIndex (K1 i c)

instance (Unsatisfiable NotAnEnumeration) => GMatIndex (f :*: g)

instance (Unsatisfiable ('Text "GDimOf: an empty type has no MatIndex.")) => GMatIndex V1

instance (GMatIndex f, GMatIndex g) => GMatIndex (f :+: g) where
  gDim = I.sPlus (gDim @f) (gDim @g)
  gToOrd (G.L1 x) = gToOrd x
  gToOrd (G.R1 x) = I.sizeOf (gDim @f) + gToOrd x
  gFromOrd i
    | i < I.sizeOf (gDim @f) = G.L1 (gFromOrd i)
    | otherwise              = G.R1 (gFromOrd (i - I.sizeOf (gDim @f)))

-- MatIndex: maps user types to Dim indices

{- | Maps user types to @Dim@ indices for typed matrix dimensions.

For an enumeration with a 'Generic' instance every method has a default, so an
empty instance is enough:

@
data Colour = R | G | B deriving ('Generic')
instance 'MatIndex' Colour
@

Hand-written instances must satisfy, for @n = 'cardinality' \@a@:

* @0 <= 'toOrd' x < n@ for every @x@;
* @'toOrd' ('fromOrd' i) == i@ for every @0 <= i < n@.

'fromOrd' '.' 'toOrd' may normalise its argument instead of returning it
unchanged (the 'BoundedList' instance sorts and removes duplicates).
-}
class MatIndex a where
  -- | The dimension tree used for this index type.
  type DimOf a :: I.Dim

  type DimOf a = GDimOf a

  -- | Runtime witness of 'DimOf'. The default reads it off the 'Generic'
  -- representation, which also checks that 'DimOf' is 'GDimOf'. A hand-written
  -- instance with a concrete 'DimOf' can use @dimOf = 'I.dimSing'@.
  dimOf :: I.SDim (DimOf a)
  default dimOf :: (GMatIndex (Rep a), DimOf a ~ GDimOf a) => I.SDim (DimOf a)
  dimOf = gDim @(Rep a)

  -- | Position of a value along the dimension, from 0.
  toOrd :: a -> Int
  default toOrd :: (Generic a, GMatIndex (Rep a)) => a -> Int
  toOrd = gToOrd . from

  -- | Value at a position along the dimension.
  fromOrd :: Int -> a
  default fromOrd :: (Generic a, GMatIndex (Rep a)) => Int -> a
  fromOrd = to . gFromOrd

-- | Number of values of an index type, i.e. the size of its dimension.
cardinality :: forall a. (MatIndex a) => Int
cardinality = I.sizeOf (dimOf @a)

instance MatIndex () where
  type DimOf () = I.U
  toOrd () = 0
  fromOrd _ = ()

instance MatIndex Bool where
  type DimOf Bool = I.U I.:+: I.U
  toOrd False = 0
  toOrd True  = 1
  fromOrd 0 = False
  fromOrd _ = True

instance (MatIndex a, MatIndex b) => MatIndex (Either a b) where
  type DimOf (Either a b) = DimOf a I.:+: DimOf b
  dimOf = I.sPlus (dimOf @a) (dimOf @b)
  toOrd (Left a)  = toOrd a
  toOrd (Right b) = cardinality @a + toOrd b
  fromOrd i
    | i < cardinality @a = Left (fromOrd i)
    | otherwise          = Right (fromOrd (i - cardinality @a))

instance (MatIndex a, MatIndex b) => MatIndex (a, b) where
  type DimOf (a, b) = I.DimProd (DimOf a) (DimOf b)
  dimOf = I.sDimProd (dimOf @a) (dimOf @b)
  toOrd (a, b) = toOrd a * cardinality @b + toOrd b
  fromOrd i = (fromOrd (div i (cardinality @b)), fromOrd (mod i (cardinality @b)))

instance (KnownNat n, KnownNat m, n <= m) => MatIndex (Ranged n m) where
  type DimOf (Ranged n m) = I.FromNat ((m - n) + 1)
  dimOf =
    unsafeSFromNat @((m - n) + 1)
      (natSize (natVal (Proxy @m) - natVal (Proxy @n) + 1))
  toOrd (Rng v) = v - fromInteger (natVal (Proxy :: Proxy n))
  fromOrd i = mkRanged (i + fromInteger (natVal (Proxy :: Proxy n)))

{- | Subsets of @a@ as an index. Numbered by bitmask with the element of
ordinal 0 as the most significant bit, which matches the 'Enum' instance of
'BoundedList' and the layout of 'I.Pow'.
-}
instance (MatIndex a) => MatIndex (BoundedList a) where
  type DimOf (BoundedList a) = I.Pow (DimOf a)
  dimOf = I.sPow (dimOf @a)
  toOrd (L xs) = foldl' (.|.) 0 [bit (n - 1 - toOrd x) | x <- xs]
    where
      n = cardinality @a
  fromOrd i = L [fromOrd j | j <- [0 .. n - 1], testBit i (n - 1 - j)]
    where
      n = cardinality @a

-- Bridges from a 'MatIndex' witness to the 'I.KnownDim' constraints the
-- Internal combinators take.

withDim :: forall a r. (MatIndex a) => ((I.KnownDim (DimOf a)) => r) -> r
withDim = I.withKnownDim (dimOf @a)

withDims :: forall a b r. (MatIndex a, MatIndex b) => ((I.KnownDim (DimOf a), I.KnownDim (DimOf b)) => r) -> r
withDims k = withDim @a (withDim @b k)

-- | Builds a matrix from an index function @(col -> row -> e)@.
generateM :: forall a b e. (MatIndex a, MatIndex b) => (Int -> Int -> e) -> Matrix e a b
generateM f = M (I.generateS (dimOf @a) (dimOf @b) f)

-- | LAoP (Linear Algebra of Programming) Inductive Matrix definition.
newtype Matrix e a b = M (I.Matrix e (DimOf a) (DimOf b))
  deriving (Eq, Ord, NFData) via (I.Matrix e (DimOf a) (DimOf b))

-- | Shows a matrix as the 'fromLists' call that builds it, whatever its layout.
instance (Show e) => Show (Matrix e a b) where
  showsPrec d m = showParen (d > 10) (showString "fromLists " . showsPrec 11 (toLists m))

-- | One type alias
type One = ()

-- Category instance

instance (Num e) => Category (Matrix e) where
  type Object (Matrix e) a = MatIndex a
  id = iden
  (.) = comp

-- Primitives

-- | Unit matrix constructor
one :: e -> Matrix e () ()
one = M . I.one

{- | Puts @a@ to the left of @b@, giving a matrix whose columns are indexed by
'Either'. Macedo and Oliveira (2013, eq. 16) call this the junc @[a|b]@.
-}
join :: Matrix e a c -> Matrix e b c -> Matrix e (Either a b) c
join (M a) (M b) = M (I.join a b)
{-# NOINLINE [1] join #-}

{- | Puts @a@ on top of @b@, with rows indexed by 'Either': the split @[a/b]@
of Macedo and Oliveira (2013, eq. 17). The pairing that fork algebras call "fork" is 'kr'.
-}
fork :: Matrix e c a -> Matrix e c b -> Matrix e c (Either a b)
fork (M a) (M b) = M (I.fork a b)
{-# NOINLINE [1] fork #-}

infixl 3 |||

{- | Matrix @Join@ constructor. An alias of 'join', inlined early so the
rewrite rules written against 'join' see through it.
-}
(|||) :: Matrix e a c -> Matrix e b c -> Matrix e (Either a b) c
(|||) = join
{-# INLINE (|||) #-}

infixl 2 ===

{- | Matrix @Fork@ constructor. An alias of 'fork', inlined early so the
rewrite rules written against 'fork' see through it.
-}
(===) :: Matrix e c a -> Matrix e c b -> Matrix e c (Either a b)
(===) = fork
{-# INLINE (===) #-}

-- Construction

{- | Build a matrix out of a list of rows. Throws a runtime error unless there
is exactly one list per row and every row has exactly one element per column.
-}
fromLists :: forall e a b. (HasCallStack, MatIndex a, MatIndex b) => [[e]] -> Matrix e a b
fromLists ls = M (withDims @a @b (I.fromLists ls))

-- | Converts a matrix to a list of lists of elements.
toLists :: Matrix e a b -> [[e]]
toLists (M m) = I.toLists m

-- | The element of a 1x1 matrix.
scalar :: Matrix e () () -> e
scalar (M (I.One e)) = e

-- | Converts a matrix to a list of elements.
toList :: Matrix e a b -> [e]
toList (M m) = I.toList m

-- | Constructs a column vector matrix
col :: forall e b. (HasCallStack, MatIndex b) => [e] -> Matrix e () b
col l = M (withDim @b (I.col l))

-- | Constructs a row vector matrix
row :: forall e a. (HasCallStack, MatIndex a) => [e] -> Matrix e a ()
row l = M (withDim @a (I.row l))

-- | The zero matrix. A matrix wholly filled with zeros.
zeros :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e a b
zeros = M (withDims @a @b I.zeros)
{-# NOINLINE [1] zeros #-}

-- | The ones matrix. A matrix wholly filled with ones.
ones :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e a b
ones = M (withDims @a @b I.ones)
{-# NOINLINE [1] ones #-}

-- | The T (Top) row vector matrix.
bang :: forall e a. (Num e, MatIndex a) => Matrix e a ()
bang = M (withDim @a I.bang)

-- | Constructs a matrix filled with a constant value.
constant :: forall e a b. (MatIndex a, MatIndex b) => e -> Matrix e a b
constant e = generateM (\_ _ -> e)

-- | Identity matrix.
iden :: forall e a. (Num e, MatIndex a) => Matrix e a a
iden = M (withDim @a I.iden)
{-# NOINLINE [1] iden #-}

-- Matrix builder

{- | Builds a matrix from a function of zero-based indices, given as
@(row, column)@, the order of the usual @M(i, j)@ notation.
-}
matrixBuilder' ::
  forall e a b.
  (MatIndex a, MatIndex b) =>
  ((Int, Int) -> e) ->
  Matrix e a b
matrixBuilder' f = generateM (\c r -> f (r, c))

{- | Builds a matrix from a function of index values. The argument is
@(column, row)@, source before target, following the type @Matrix e a b@ (the
opposite order to 'matrixBuilder'').
-}
matrixBuilder ::
  forall e a b.
  (MatIndex a, MatIndex b) =>
  ((a, b) -> e) ->
  Matrix e a b
matrixBuilder f = generateM (\c r -> f (fromOrd c, fromOrd r))

-- Lifting functions

-- | Lifts functions to matrices with dimensions matching @a@ and @b@.
fromF ::
  forall e a b.
  (MatIndex a, MatIndex b, Num e) =>
  (a -> b) ->
  Matrix e a b
fromF f = generateM (\c r -> if target ! c == r then 1 else 0)
  where
    -- f is applied once per column, not once per cell.
    n = cardinality @a
    target = listArray (0, n - 1) [toOrd (f (fromOrd c)) | c <- [0 .. n - 1]] :: Array Int Int

-- | Lifts relation functions to Boolean Matrix
toRel ::
  forall a b.
  (MatIndex a, MatIndex b) =>
  (a -> b -> Bool) ->
  Matrix I.Boolean a b
toRel f = generateM (\c r -> I.fromBool (f (fromOrd c) (fromOrd r)))

{- | Bifunctor equivalent function: relabels the columns of a matrix through
@f@ and its rows through @g@.

@
'bimapM' f g m == 'fromF' g '.' m '.' 'tr' ('fromF' f)
@
-}
bimapM ::
  (MatIndex a, MatIndex b, MatIndex c, MatIndex d, Num e) =>
  (a -> b) ->
  (c -> d) ->
  Matrix e a c ->
  Matrix e b d
bimapM f g m = fromF g `comp` m `comp` tr (fromF f)

-- Applicative-like

-- | Applicative instance equivalent @unit@ function.
unitM :: (Num e) => Matrix e () ()
unitM = one 1

-- | Applicative instance equivalent @mult@ function.
multM ::
  (Num e) =>
  Matrix e c a ->
  Matrix e c b ->
  Matrix e c (a, b)
multM = kr

-- Selective

{- | Selective functors 'select' operator equivalent inspired by the
ArrowMonad instance in Selective Applicative Functors (Mokhov et al. 2019).
-}
selectM ::
  (Num e) =>
  Matrix e c (Either a b) ->
  Matrix e a b ->
  Matrix e c b
selectM = select

-- Point

-- | Point constant relation
point ::
  forall e a.
  (Num e, MatIndex a) =>
  a ->
  Matrix e () a
point a = generateM (\_ r -> if r == toOrd a then 1 else 0)

-- Composition

{- | Matrix composition: @comp f g@ applies @g@ and then @f@. It is
'LAoP.Matrix.Internal.comp', whose documentation describes how the product is
computed, and the composition of the 'Category' instance.

Optimised builds rewrite some compositions away with the rules of this module,
the rules of "LAoP.Matrix.Internal#rules" restated for these matrices
(@comp m iden@ becomes @m@, @comp p1 (fork a b)@ becomes @a@, and so on). The
rules assume exact arithmetic: if a matrix holds @NaN@ or an infinity, the
result can differ from an unoptimised build, because the skipped products by
zero would have propagated it.
-}
comp :: (Num e) => Matrix e b c -> Matrix e a b -> Matrix e a c
comp (M a) (M b) = M (I.comp a b)
{-# NOINLINE [1] comp #-}

-- The parallel wrappers are coercions with no arguments of their own, so they
-- inline even when partially applied and reach the specialisations of
-- 'I.parCompWith' at every call site.

-- | 'comp' on several cores, equal to it bit for bit. See 'I.parComp' for how the depth is chosen.
parComp :: forall e b c a. (Num e) => Matrix e b c -> Matrix e a b -> Matrix e a c
parComp = coerce (I.parComp @e @(DimOf b) @(DimOf c) @(DimOf a))
{-# INLINE parComp #-}

-- | 'comp' with at most @depth@ levels of parallel splits. See 'I.parCompWith'.
parCompWith :: forall e b c a. (Num e) => Int -> Matrix e b c -> Matrix e a b -> Matrix e a c
parCompWith = coerce (I.parCompWith @e @(DimOf b) @(DimOf c) @(DimOf a))
{-# INLINE parCompWith #-}

{-# RULES
-- Category: identity
"Indexed comp/iden-right" forall m. comp m iden = m
"Indexed comp/iden-left"  forall m. comp iden m = m

-- Transpose: involution and constants
"Indexed tr/involution" forall m. tr (tr m) = m
"Indexed tr/iden"   tr iden = iden

-- Additive identity
"Indexed add/zeros-right" forall m. m .+. zeros = m
"Indexed add/zeros-left"  forall m. zeros .+. m = m

-- Hadamard identity and annihilation
"Indexed had/ones-right"  forall m. m .*. ones = m
"Indexed had/ones-left"   forall m. ones .*. m = m
"Indexed had/zeros-right" forall m. m .*. zeros = zeros
"Indexed had/zeros-left"  forall m. zeros .*. m = zeros

-- Biproduct (Macedo and Oliveira 2013, eqs. 11, 12) and orthogonality (eqs. 14, 15)
"Indexed comp/p1-i1" comp p1 i1 = iden
"Indexed comp/p2-i2" comp p2 i2 = iden
"Indexed comp/p1-i2" comp p1 i2 = zeros
"Indexed comp/p2-i1" comp p2 i1 = zeros

-- Cancellation (Macedo and Oliveira 2013, eqs. 28, 29)
"Indexed comp/p1-fork" forall a b. comp p1 (fork a b) = a
"Indexed comp/p2-fork" forall a b. comp p2 (fork a b) = b
"Indexed comp/join-i1" forall a b. comp (join a b) i1 = a
"Indexed comp/join-i2" forall a b. comp (join a b) i2 = b
  #-}

-- Transposition

-- | Matrix transposition.
tr :: Matrix e a b -> Matrix e b a
tr (M m) = M (I.tr m)
{-# NOINLINE [1] tr #-}

-- Element-wise

infixl 6 .+.

-- | Element-wise matrix addition.
(.+.) :: (Num e) => Matrix e a b -> Matrix e a b -> Matrix e a b
(.+.) (M a) (M b) = M (a I..+. b)
{-# NOINLINE [1] (.+.) #-}

infixl 6 .-.

-- | Element-wise matrix subtraction.
(.-.) :: (Num e) => Matrix e a b -> Matrix e a b -> Matrix e a b
(.-.) (M a) (M b) = M (a I..-. b)

infixl 7 .*.

-- | Element-wise matrix multiplication (Hadamard product).
(.*.) :: (Num e) => Matrix e a b -> Matrix e a b -> Matrix e a b
(.*.) (M a) (M b) = M (a I..*. b)
{-# NOINLINE [1] (.*.) #-}

infixl 7 .|

-- | Scalar multiplication of matrices.
(.|) :: (Num e) => e -> Matrix e a b -> Matrix e a b
(.|) s (M m) = M (s I..| m)

infixl 7 ./

-- | Scalar division of matrices.
(./) :: (Fractional e) => Matrix e a b -> e -> Matrix e a b
(./) (M m) s = M (m I../ s)

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e a b -> Matrix f a b -> Matrix g a b
zipWithM f (M a) (M b) = M (I.zipWithM f a b)

-- | Applies a function to every element.
emap :: (e -> f) -> Matrix e a b -> Matrix f a b
emap f (M m) = M (I.emap f m)

-- Biproduct

{- | Projects the 'Left' block of rows, @[id|0]@ in the standard biproduct of
Macedo and Oliveira (2013, eq. 23). Pairs are projected by 'fstM' instead.
-}
p1 :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e (Either a b) a
p1 = M (withDims @a @b I.p1)
{-# NOINLINE [1] p1 #-}

-- | Projects the 'Right' block of rows, @[0|id]@.
p2 :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e (Either a b) b
p2 = M (withDims @a @b I.p2)
{-# NOINLINE [1] p2 #-}

-- | Injects into the 'Left' block, @[id/0]@; the transpose of 'p1'.
i1 :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e a (Either a b)
i1 = M (withDims @a @b I.i1)
{-# NOINLINE [1] i1 #-}

-- | Injects into the 'Right' block, @[0/id]@.
i2 :: forall e a b. (Num e, MatIndex a, MatIndex b) => Matrix e b (Either a b)
i2 = M (withDims @a @b I.i2)
{-# NOINLINE [1] i2 #-}

infixl 5 -|-

{- | Direct sum, @a@ and @b@ placed on the diagonal of a block matrix with
zeros elsewhere. It is the biproduct bifunctor, which makes it the coproduct and
the product functor at once (Macedo and Oliveira 2013, eq. 62):
@a -|- b == join (i1 . a) (i2 . b)@.
-}
(-|-) ::
  (Num e) =>
  Matrix e a b ->
  Matrix e c d ->
  Matrix e (Either a c) (Either b d)
(-|-) (M a) (M b) = M (a I.-|- b)

{- | Projects a pair onto its first component. Macedo and Oliveira (2013,
sec. 13) call this Khatri-Rao projection @p1@.
-}
fstM ::
  forall e a b.
  (Num e, MatIndex a, MatIndex b) =>
  Matrix e (a, b) a
fstM = M (withDims @a @b (I.fstM @e @(DimOf a) @(DimOf b)))

-- | Projects a pair onto its second component (Macedo and Oliveira's @p2@).
sndM ::
  forall e a b.
  (Num e, MatIndex a, MatIndex b) =>
  Matrix e (a, b) b
sndM = M (withDims @a @b (I.sndM @e @(DimOf a) @(DimOf b)))

{- | Khatri Rao Matrix product also known as matrix pairing.

The laws below hold for column-stochastic matrices, that is probabilistic
functions (functions included), where Khatri-Rao is a weak product (Murta and
Oliveira 2013, eq. 22). For other matrices each projection scales its factor by
the column sums of the other.

  NOTE: This is not a true categorical product, see for instance:

@
           | fstM . kr a b == a
kr a b ==> |
           | sndM . kr a b == b
@

__Emphasis__ on the implication symbol.
-}
kr ::
  (Num e) =>
  Matrix e c a ->
  Matrix e c b ->
  Matrix e c (a, b)
kr (M a) (M b) = M (I.kr a b)

infixl 4 ><

{- | Kronecker product, which replaces each element @x@ of @a@ by the block
@x .| b@. It is the tensor product of matrices; the categorical product is the
biproduct (@-|-@, 'fork'). Macedo and Oliveira (2013, sec. 13) express it through
'kr' as @kr (a . fstM) (b . sndM)@, and the computation follows their Kronecker
fusion laws (eqs. 60, 61), which distribute @><@ over 'join' and 'fork' on the
left.

Macedo and Oliveira let products bind tighter than sums, but @><@ is @infixl 4@:
@a >< b .+. c@ is @a >< (b .+. c)@, and a comparison such as @(a >< b) == c@
needs the parentheses.
-}
(><) ::
  (Num e) =>
  Matrix e a c ->
  Matrix e b d ->
  Matrix e (a, b) (c, d)
(><) (M a) (M b) = M (a I.>< b)

-- Abide

{- | Applies the junc/split exchange ("abide") law (Macedo and Oliveira 2013,
eq. 30) from left to right wherever it matches,

@
join (fork a c) (fork b d) == fork (join a b) (join c d)
@

which changes how the matrix is laid out but none of its elements.
-}
abideJF :: Matrix e a b -> Matrix e a b
abideJF (M m) = M (I.abideJF m)

{- | The inverse rewrite of 'abideJF': each @fork (join a b) (join c d)@
becomes @join (fork a c) (fork b d)@. The result equals the argument.
-}
abideFJ :: Matrix e a b -> Matrix e a b
abideFJ (M m) = M (I.abideFJ m)

-- Block decomposition

{- | Splits a matrix into its two column blocks, whatever its layout, so
@splitJoin (join a b) == (a, b)@. The blocks are @(comp m i1, comp m i2)@, but
neither product is computed: a matrix built by 'join' splits in O(1), and any
other layout rebuilds only the row structure above the boundary.

For example, @m@ below has its columns indexed by @Either () Bool@ and its
rows by 'Bool'. 'fromLists' takes one list per row, top to bottom, and the
columns in 'MatIndex' order:

@
          Left ()  |  Right False  Right True
  False      1     |       2            3
  True       4     |       5            6
@

'splitJoin' cuts at the bar: the @Left ()@ column on one side, the two
@Right@ columns on the other. The matrix is stored row by row, not as a
'join', and the split works all the same:

>>> let m = fromLists [[1, 2, 3], [4, 5, 6]] :: Matrix Int (Either () Bool) Bool
>>> let (l, r) = splitJoin m
>>> toLists l
[[1],[4]]
>>> toLists r
[[2,3],[5,6]]
-}
splitJoin :: Matrix e (Either a b) c -> (Matrix e a c, Matrix e b c)
splitJoin (M m) = let (x, y) = I.splitJoin m in (M x, M y)

{- | Splits a matrix into its top and bottom row blocks, undoing 'fork'. The
result equals @(comp p1 m, comp p2 m)@ and costs what 'splitJoin' costs, with
rows and columns swapped.

For example, @m@ below has its rows indexed by @Either () Bool@ and its columns
by 'Bool':

@
               False  True
  Left ()        1      2
  -------------------------
  Right False    3      4
  Right True     5      6
@

'splitFork' cuts at the line: the @Left ()@ row on top, the two @Right@ rows
below:

>>> let m = fromLists [[1, 2], [3, 4], [5, 6]] :: Matrix Int Bool (Either () Bool)
>>> let (t, b) = splitFork m
>>> toLists t
[[1,2]]
>>> toLists b
[[3,4],[5,6]]
-}
splitFork :: Matrix e c (Either a b) -> (Matrix e c a, Matrix e c b)
splitFork (M m) = let (x, y) = I.splitFork m in (M x, M y)

-- Select/Branch/Cond

{- | Selective functors 'select' operator equivalent inspired by the
ArrowMonad instance in Selective Applicative Functors (Mokhov et al. 2019).
-}
select ::
  (Num e) =>
  Matrix e c (Either a b) ->
  Matrix e a b ->
  Matrix e c b
select (M m) (M y) = M (I.select m y)

{- | Selective functors 'branch' operator: the left component of the input is
sent through the first matrix, the right component through the second.
-}
branch ::
  (Num e) =>
  Matrix e d (Either a b) ->
  Matrix e a c ->
  Matrix e b c ->
  Matrix e d c
branch (M x) (M l) (M r) = M (I.branch x l r)

{- | McCarthy's conditional: column @x@ of the result is column @x@ of the
first matrix when @p x@ holds and of the second one otherwise. The predicate is
evaluated once per column.
-}
cond ::
  forall e a b.
  (MatIndex a, MatIndex b) =>
  (a -> Bool) ->
  Matrix e a b ->
  Matrix e a b ->
  Matrix e a b
cond p (M f) (M g) =
  let n = cardinality @a
      takeFirst = listArray (0, n - 1) [p (fromOrd c) | c <- [0 .. n - 1]] :: Array Int Bool
      M choice = generateM @a @b (\c _ -> takeFirst ! c)
      pick s x y = if s then x else y
   in M (I.zipWithM ($) (I.zipWithM pick choice f) g)

-- Dimensions

-- | Number of columns, read from the column index type.
columns :: forall e a b. (MatIndex a) => Matrix e a b -> Int
columns _ = cardinality @a

-- | Number of rows, read from the row index type.
rows :: forall e a b. (MatIndex b) => Matrix e a b -> Int
rows _ = cardinality @b

-- Pretty printing

-- | Matrix pretty printer
pretty :: (Show e) => Matrix e a b -> String
pretty (M m) = I.pretty m

-- | Matrix pretty printer
prettyPrint :: (Show e) => Matrix e a b -> IO ()
prettyPrint = putStrLn . pretty
