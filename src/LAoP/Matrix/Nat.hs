{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Nat
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrices indexed by type-level natural numbers, for code that thinks in sizes
rather than in index types. This module wraps 'LAoP.Matrix.Internal.Matrix' as
"LAoP.Matrix.Indexed" does, with type-level naturals for the dimensions. A @'Matrix' e c r@ has @c@ columns and @r@ rows, and every
dimension constraint is 'Dimension', a 'KnownNat' of at least 1.

Internally a dimension @n@ is the balanced tree @'I.FromNat' n@, split at
@n \`div\` 2@. Block operations ('join', 'fork', the projections and injections,
'kr', '><') accept any sizes, and lay their result out along that tree. When
the blocks already line up with it, that costs O(size of the trees) on top of
the operation itself; otherwise it costs O(size of the matrix), so building a
matrix by repeatedly joining single columns is quadratic.
-}
module LAoP.Matrix.Nat (
  -- * Matrix type
  Matrix (..),

  -- * Dimension constraint
  Dimension,
  AtLeastOne,
  I.FromNat,

  -- * Primitives
  one,
  join,
  fork,

  -- * Block decomposition
  splitJoin,
  splitFork,

  -- * Construction
  fromLists,
  toLists,
  toList,
  scalar,
  matrixBuilder',
  fromF,
  point,
  col,
  row,
  zeros,
  ones,
  bang,
  constant,

  -- * Composition and transposition
  iden,
  comp,
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

  -- * Pairing
  fstM,
  sndM,
  kr,

  -- * Selective and conditional
  select,
  branch,
  cond,

  -- * Matrix "abiding"
  abideJF,
  abideFJ,

  -- * Dimensions
  columns,
  rows,

  -- * Pretty printing
  pretty,
  prettyPrint,
) where

import           Control.DeepSeq
import           Data.Array               (Array, listArray, (!))
import           Data.Kind                (Constraint)
import           Data.Proxy               (Proxy (..))
import           GHC.Stack                (HasCallStack)
import           GHC.TypeLits
import           LAoP.Category            (Category (..))
import qualified LAoP.Matrix.Internal     as I
import           LAoP.Matrix.Internal.Dim (natSize, unsafeSFromNat)
import           Prelude                  hiding (id, (.))

-- | Matrix with @cols@ columns and @rows@ rows.
newtype Matrix e (cols :: Nat) (rows :: Nat) = M (I.Matrix e (I.FromNat cols) (I.FromNat rows))
  deriving (Eq, Ord, NFData) via (I.Matrix e (I.FromNat cols) (I.FromNat rows))

-- | Shows a matrix as the 'fromLists' call that builds it, whatever its layout.
instance (Show e) => Show (Matrix e cols rows) where
  showsPrec d m = showParen (d > 10) (showString "fromLists " . showsPrec 11 (toLists m))

-- | Matrices form a category whose objects are the known naturals.
instance (Num e) => Category (Matrix e) where
  type Object (Matrix e) n = Dimension n
  id = iden
  (.) = comp

-- Dimensions

{- | A dimension of a matrix: a known natural of at least 1. A 0 is rejected
when the program is type checked:

@
iden \@Double \@0   -- LAoP.Matrix.Nat: the dimension 0 must be at least 1
@

Polymorphic code carries the constraint as it would carry 'KnownNat'.
-}
type Dimension n = (KnownNat n, AtLeastOne n)

{- | The check behind 'Dimension': no constraint for a natural of at least 1,
a type error for 0.
-}
type family AtLeastOne (n :: Nat) :: Constraint where
  AtLeastOne 0 = TypeError ('Text "LAoP.Matrix.Nat: the dimension 0 must be at least 1")
  AtLeastOne _ = ()

-- Dimension witnesses

natInt :: forall n. (KnownNat n) => Int
natInt = natSize (natVal (Proxy @n))

-- Bridges a 'KnownNat' to the 'I.KnownDim' of its dimension tree.
withNat :: forall n r. (KnownNat n) => ((I.KnownDim (I.FromNat n)) => r) -> r
withNat = I.withKnownDim (I.sFromNat @n)

withNats :: forall a b r. (KnownNat a, KnownNat b) => ((I.KnownDim (I.FromNat a), I.KnownDim (I.FromNat b)) => r) -> r
withNats k = withNat @a (withNat @b k)

-- The tree of @a + b@, computed from the two summands (no KnownNat (a + b)).
sFromNatSum :: forall a b. (KnownNat a, KnownNat b) => I.SDim (I.FromNat (a + b))
sFromNatSum = unsafeSFromNat @(a + b) (natSize (natVal (Proxy @a) + natVal (Proxy @b)))

-- The tree of @a * b@, computed from the two factors.
sFromNatProd :: forall a b. (KnownNat a, KnownNat b) => I.SDim (I.FromNat (a * b))
sFromNatProd = unsafeSFromNat @(a * b) (natSize (natVal (Proxy @a) * natVal (Proxy @b)))

-- The tree of @a + b@ as the sum of the two summands' trees.
sSplit :: forall a b. (KnownNat a, KnownNat b) => I.SDim (I.FromNat a I.:+: I.FromNat b)
sSplit = I.sPlus (I.sFromNat @a) (I.sFromNat @b)

-- Primitives

-- | The 1x1 matrix holding one element.
one :: e -> Matrix e 1 1
one = M . I.One

{- | Places two matrices with the same rows side by side (the @Join@ block
constructor, the junc @[a|b]@ of Macedo and Oliveira 2013).
-}
join ::
  forall e a b rows.
  (Dimension a, Dimension b) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (a + b) rows
join (M x) (M y) = M (I.relayout (sFromNatSum @a @b) (I.rowShape x) (I.Join x y))

infixl 3 |||

-- | Operator form of 'join'.
(|||) ::
  (Dimension a, Dimension b) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (a + b) rows
(|||) = join

{- | Stacks two matrices with the same columns one above the other (the
@Fork@ block constructor, the split @[a/b]@ of Macedo and Oliveira 2013).
-}
fork ::
  forall e cols a b.
  (Dimension a, Dimension b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a + b)
fork (M x) (M y) = M (I.relayout (I.colShape x) (sFromNatSum @a @b) (I.Fork x y))

infixl 2 ===

-- | Operator form of 'fork'.
(===) ::
  (Dimension a, Dimension b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a + b)
(===) = fork

-- Block decomposition

{- | Takes the first @a@ columns and the remaining @b@, undoing 'join'. The
matrix is first laid out as @a@ columns beside @b@ columns, which is free when
that split is where its tree already splits and O(size of the matrix)
otherwise.

For example, @m@ below has 3 columns and 2 rows ('fromLists' takes one list
per row):

@
1 2 3
4 5 6
@

The type applications choose where to cut, after the first column or after the
second, and 'toLists' shows each block row by row. The tree of 3 is @1 + 2@, so
the first cut is free and the second lays the matrix out again:

>>> let m = fromLists [[1, 2, 3], [4, 5, 6]] :: Matrix Int 3 2
>>> let (l, r) = splitJoin @Int @1 @2 m
>>> (toLists l, toLists r)
([[1],[4]],[[2,3],[5,6]])
>>> let (l', r') = splitJoin @Int @2 @1 m
>>> (toLists l', toLists r')
([[1,2],[4,5]],[[3],[6]])
-}
splitJoin ::
  forall e a b rows.
  (Dimension a, Dimension b) =>
  Matrix e (a + b) rows ->
  (Matrix e a rows, Matrix e b rows)
splitJoin (M m) =
  let (x, y) = I.splitJoin (I.relayout (sSplit @a @b) (I.rowShape m) m)
   in (M x, M y)

{- | Takes the first @a@ rows and the remaining @b@, undoing 'fork', at the
same cost as 'splitJoin'. For example, cutting the 3 rows of

@
1 2
3 4
5 6
@

after the first row:

>>> let m = fromLists [[1, 2], [3, 4], [5, 6]] :: Matrix Int 2 3
>>> let (t, b) = splitFork @Int @2 @1 @2 m
>>> (toLists t, toLists b)
([[1,2]],[[3,4],[5,6]])
-}
splitFork ::
  forall e cols a b.
  (Dimension a, Dimension b) =>
  Matrix e cols (a + b) ->
  (Matrix e cols a, Matrix e cols b)
splitFork (M m) =
  let (x, y) = I.splitFork (I.relayout (I.colShape m) (sSplit @a @b) m)
   in (M x, M y)

-- Construction

{- | Builds a matrix from a list of rows. Throws a runtime error unless there
is exactly one list per row and every row has exactly one element per column.
-}
fromLists ::
  forall e cols rows.
  (HasCallStack, Dimension cols, Dimension rows) =>
  [[e]] -> Matrix e cols rows
fromLists ls = M (withNats @cols @rows (I.fromLists ls))

{- | Builds a matrix from a function of the zero-based @(row, column)@
position.
-}
matrixBuilder' ::
  forall e cols rows.
  (Dimension cols, Dimension rows) =>
  ((Int, Int) -> e) ->
  Matrix e cols rows
matrixBuilder' f = M (I.generateS (I.sFromNat @cols) (I.sFromNat @rows) (\c r -> f (r, c)))

{- | Lifts a function on zero-based indices to a matrix: column @c@ has a 1 in
row @f c@ and 0 elsewhere. The function is applied once per column; results
outside @[0, rows)@ give an all-zero column.
-}
fromF ::
  forall e cols rows.
  (Num e, Dimension cols, Dimension rows) =>
  (Int -> Int) ->
  Matrix e cols rows
fromF f = matrixBuilder' (\(r, c) -> if target ! c == r then 1 else 0)
  where
    n = natInt @cols
    target = listArray (0, n - 1) (map f [0 .. n - 1]) :: Array Int Int

-- | Column vector with a 1 at the given zero-based row.
point :: forall e rows. (Num e, Dimension rows) => Int -> Matrix e 1 rows
point i = matrixBuilder' (\(r, _) -> if r == i then 1 else 0)

-- | Converts a matrix to a list of rows.
toLists :: Matrix e cols rows -> [[e]]
toLists (M m) = I.toLists m

-- | The element of a 1x1 matrix.
scalar :: Matrix e 1 1 -> e
scalar (M (I.One e)) = e

-- | Converts a matrix to its elements in row-major order.
toList :: Matrix e cols rows -> [e]
toList (M m) = I.toList m

-- | Column vector from a list with one element per row.
col ::
  forall e rows.
  (HasCallStack, Dimension rows) =>
  [e] -> Matrix e 1 rows
col l = M (withNat @rows (I.col l))

-- | Row vector from a list with one element per column.
row ::
  forall e cols.
  (HasCallStack, Dimension cols) =>
  [e] -> Matrix e cols 1
row l = M (withNat @cols (I.row l))

-- | The zero matrix.
zeros ::
  forall e cols rows.
  (Num e, Dimension cols, Dimension rows) =>
  Matrix e cols rows
zeros = M (withNats @cols @rows I.zeros)

-- | The matrix filled with ones, also known as T (top).
ones ::
  forall e cols rows.
  (Num e, Dimension cols, Dimension rows) =>
  Matrix e cols rows
ones = M (withNats @cols @rows I.ones)

-- | A matrix filled with one value.
constant ::
  forall e cols rows.
  (Dimension cols, Dimension rows) =>
  e -> Matrix e cols rows
constant e = M (withNats @cols @rows (I.constant e))

-- | The row vector of ones.
bang ::
  forall e cols.
  (Num e, Dimension cols) =>
  Matrix e cols 1
bang = M (withNat @cols I.bang)

-- | The identity matrix.
iden ::
  forall e cols.
  (Num e, Dimension cols) =>
  Matrix e cols cols
iden = M (withNat @cols I.iden)

-- Composition

-- | Matrix multiplication, read right to left: @comp a b@ applies @b@ first.
-- It is 'LAoP.Matrix.Internal.comp', whose documentation describes how the
-- product is computed.
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)

-- Element-wise

infixl 6 .+.

-- | Element-wise addition.
(.+.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.+.) (M a) (M b) = M (a I..+. b)

infixl 6 .-.

-- | Element-wise subtraction.
(.-.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.-.) (M a) (M b) = M (a I..-. b)

infixl 7 .*.

-- | Element-wise multiplication (Hadamard product).
(.*.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.*.) (M a) (M b) = M (a I..*. b)

-- Scalar

infixl 7 .|

-- | Scalar multiplication of matrices.
(.|) :: (Num e) => e -> Matrix e cols rows -> Matrix e cols rows
(.|) e (M m) = M (e I..| m)

infixl 7 ./

-- | Scalar division of matrices.
(./) :: (Fractional e) => Matrix e cols rows -> e -> Matrix e cols rows
(./) (M m) e = M (m I../ e)

-- Transposition

-- | Matrix transposition
tr :: Matrix e cols rows -> Matrix e rows cols
tr (M m) = M (I.tr m)

-- Biproduct

-- | Projection onto the first @m@ rows of an @m + n@ vector.
p1 ::
  forall e m n.
  (Num e, Dimension m, Dimension n) =>
  Matrix e (m + n) m
p1 = M (I.relayout (sFromNatSum @m @n) (I.sFromNat @m) (withNats @m @n (I.p1 @e @(I.FromNat m) @(I.FromNat n))))

-- | Projection onto the last @n@ rows of an @m + n@ vector.
p2 ::
  forall e m n.
  (Num e, Dimension m, Dimension n) =>
  Matrix e (m + n) n
p2 = M (I.relayout (sFromNatSum @m @n) (I.sFromNat @n) (withNats @m @n (I.p2 @e @(I.FromNat m) @(I.FromNat n))))

-- | Injection of an @m@ vector into the first rows of an @m + n@ vector.
i1 ::
  forall e m n.
  (Num e, Dimension m, Dimension n) =>
  Matrix e m (m + n)
i1 = tr (p1 @e @m @n)

-- | Injection of an @n@ vector into the last rows of an @m + n@ vector.
i2 ::
  forall e m n.
  (Num e, Dimension m, Dimension n) =>
  Matrix e n (m + n)
i2 = tr (p2 @e @m @n)

infixl 5 -|-

-- | Direct sum: the block-diagonal matrix with @a@ above-left and @b@ below-right.
(-|-) ::
  forall e n k m j.
  (Num e, Dimension n, Dimension m, Dimension k, Dimension j) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (n + m) (k + j)
(-|-) (M a) (M b) = M (I.relayout (sFromNatSum @n @m) (sFromNatSum @k @j) (a I.-|- b))

-- Khatri-Rao

-- | Khatri-Rao first projection: maps pair @(i, j)@ to @i@.
fstM ::
  forall e m k.
  (Num e, Dimension m, Dimension k) =>
  Matrix e (m * k) m
fstM = M (I.relayout (sFromNatProd @m @k) (I.sFromNat @m) (withNats @m @k (I.fstM @e @(I.FromNat m) @(I.FromNat k))))

-- | Khatri-Rao second projection: maps pair @(i, j)@ to @j@.
sndM ::
  forall e m k.
  (Num e, Dimension m, Dimension k) =>
  Matrix e (m * k) k
sndM = M (I.relayout (sFromNatProd @m @k) (I.sFromNat @k) (withNats @m @k (I.sndM @e @(I.FromNat m) @(I.FromNat k))))

{- | Khatri-Rao product (matrix pairing): row @(i, j)@ of the result is row @i@
of the first matrix times row @j@ of the second, element by element.
-}
kr ::
  forall e cols a b.
  (Num e, Dimension a, Dimension b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a * b)
kr (M a) (M b) = M (I.relayout (I.colShape a) (sFromNatProd @a @b) (I.kr a b))

infixl 4 ><

{- | Kronecker product. It is @infixl 4@, looser than @.+.@ and @.*.@, so
@a >< b .+. c@ is @a >< (b .+. c)@.
-}
(><) ::
  forall e m p n q.
  (Num e, Dimension m, Dimension n, Dimension p, Dimension q) =>
  Matrix e m p ->
  Matrix e n q ->
  Matrix e (m * n) (p * q)
(><) (M a) (M b) = M (I.relayout (sFromNatProd @m @n) (sFromNatProd @p @q) (a I.>< b))

-- Selective

{- | Selective functors @select@: the first @a@ rows of the input go through
the matrix, the last @b@ rows pass through unchanged, and the two are added.
-}
select ::
  forall e cols a b.
  (Num e, Dimension a, Dimension b) =>
  Matrix e cols (a + b) ->
  Matrix e a b ->
  Matrix e cols b
select (M m) (M y) = M (I.select (I.relayout (I.colShape m) (sSplit @a @b) m) y)

{- | Selective functors @branch@: the first @a@ rows of the input go through the
first matrix, the last @b@ rows through the second, and the results are added.
-}
branch ::
  forall e cols a b c.
  (Num e, Dimension a, Dimension b) =>
  Matrix e cols (a + b) ->
  Matrix e a c ->
  Matrix e b c ->
  Matrix e cols c
branch (M m) (M l) (M r) = M (I.branch (I.relayout (I.colShape m) (sSplit @a @b) m) l r)

{- | McCarthy's conditional: column @c@ of the result is column @c@ of the
first matrix when @p c@ holds (zero-based) and of the second one otherwise.
-}
cond ::
  forall e cols rows.
  (Dimension cols) =>
  (Int -> Bool) ->
  Matrix e cols rows ->
  Matrix e cols rows ->
  Matrix e cols rows
cond p (M f) (M g) =
  let n = natInt @cols
      takeFirst = listArray (0, n - 1) (map p [0 .. n - 1]) :: Array Int Bool
      choice = I.generateS (I.colShape f) (I.rowShape f) (\c _ -> takeFirst ! c)
      pick s x y = if s then x else y
   in M (I.zipWithM ($) (I.zipWithM pick choice f) g)

-- Abide

{- | Lays the matrix out again by the junc/split exchange ("abide") law of
Macedo and Oliveira (2013, eq. 30): @join (fork a c) (fork b d)@ becomes
@fork (join a b) (join c d)@. The elements do not change.
-}
abideJF :: Matrix e cols rows -> Matrix e cols rows
abideJF (M m) = M (I.abideJF m)

-- | 'abideJF' in the opposite direction, from a fork of joins to a join of forks.
abideFJ :: Matrix e cols rows -> Matrix e cols rows
abideFJ (M m) = M (I.abideFJ m)

-- Dimensions

-- | Number of columns.
columns :: forall e cols rows. (Dimension cols) => Matrix e cols rows -> Int
columns _ = natInt @cols

-- | Number of rows.
rows :: forall e cols rows. (Dimension rows) => Matrix e cols rows -> Int
rows _ = natInt @rows

-- Zip

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e cols rows -> Matrix f cols rows -> Matrix g cols rows
zipWithM f (M a) (M b) = M (I.zipWithM f a b)

-- | Applies a function to every element.
emap :: (e -> f) -> Matrix e cols rows -> Matrix f cols rows
emap f (M m) = M (I.emap f m)

-- Pretty printing

-- | Renders a matrix as a boxed grid.
pretty :: (Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

-- | Prints 'pretty' to standard output.
prettyPrint :: (Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m
