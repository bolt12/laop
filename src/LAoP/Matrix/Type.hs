{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module     : LAoP.Matrix.Type
-- Copyright  : (c) Armando Santos 2019-2020
-- Maintainer : armandoifsantos@gmail.com
-- Stability  : experimental
--
-- The LAoP discipline generalises relations and functions treating them as
-- Boolean matrices and in turn consider these as arrows.
--
-- __LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
-- in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module offers a newtype wrapper around 'Matrix.Type.Matrix' that
-- uses arbitrary types instead of canonical data types for the matrices
-- dimensions.
--
-- __NOTE__: If the types in the dimensions are custom they must need to
-- implement a 'Generic' instance.
--
-----------------------------------------------------------------------------

module LAoP.Matrix.Type
  ( -- | LAoP (Linear Algebra of Programming) Inductive Matrix definition.
    --
    --   LAoP generalises relations and functions treating them as
    --   Boolean matrices and in turn consider these as arrows.
    --   This library offers many of the combinators mentioned in the work of
    --   Macedo (2012) and Oliveira (2012).
    --
    --   This definition is a wrapper around 'Matrix.Type' but
    --   dimensions are arbitrary data types. Type inference might not
    --   be as desired.

    -- __NOTE__: If the types in the dimensions are custom they must need to
    -- implement a 'Generic' instance.
    --

    -- * Type safe matrix representation
    Matrix (..),

    -- * Constraint type aliases
    Countable,
    CountableDimensions,
    CountableN,
    CountableDimensionsN,
    FromListsN,
    Liftable,
    Trivial,
    Trivial2,
    TrivialP,
    EqN,
    EnumN,
    Enumerable,
    EnumerableN,
    ConstructNorm,
    ConstructN,
    ConstructableN,
    FstM,
    SndM,
    Kronecker,

    -- * Type aliases
    Zero,
    One,

    -- * Primitives
    empty,
    one,
    join,
    fork,

    -- * Auxiliary type families
    I.FromNat,
    I.Count,
    I.Normalize,

    -- * Matrix construction and conversion
    I.FromLists,
    fromLists,
    toLists,
    toList,
    matrixBuilder,
    rowL,
    row,
    colL,
    col,
    zeros,
    ones,
    bang,
    point,
    constant,

    -- * Functor instance equivalent function
    fmapM,
    bimapM,

    -- * Applicative/Monoidal instance equivalent functions
    unitM,
    multM,

    -- * Selective equivalent instance function
    selectM, 
    
    -- * Monad equivalent instance function
    returnM,
    bindM,

    -- * Misc
    -- ** Get dimensions
    columns,
    rows,

    -- ** Matrix Transposition
    tr,

    -- ** Scalar multiplication/division of matrices
    (.|),
    (./),

    -- ** McCarthy's Conditional
    -- cond,

    -- ** Matrix "abiding"
    abideJF,
    abideFJ,

    -- ** Zip Matrices
    zipWithM,

    -- * Biproduct approach
    -- ** Fork
    (===),
    -- *** Projections
    p1,
    p2,
    -- ** Join
    (|||),
    -- *** Injections
    i1,
    i2,
    -- ** Bifunctors
    (-|-),
    (><),

    -- ** Applicative matrix combinators

    -- | Note that given the restrictions imposed it is not possible to
    -- implement the standard type classes present in standard Haskell.
    -- *** Matrix pairing projections
    fstM,
    sndM,

    -- *** Matrix pairing
    kr,

    -- * Matrix composition and lifting

    -- ** Arrow matrix combinators

    -- | Note that given the restrictions imposed it is not possible to
    -- implement the standard type classes present in standard Haskell.
    iden,
    comp,
    fromF,

    -- * Relation
    toRel,

    -- * Matrix printing
    pretty,
    prettyPrint
  )
where

import Data.Void
import Data.Kind
import GHC.TypeLits
import Control.DeepSeq
import LAoP.Utils
import qualified LAoP.Matrix.Alternative as I
import Prelude hiding (id, (.))

newtype Matrix e (cols :: Type) (rows :: Type) = M (I.Matrix e (I.Normalize cols) (I.Normalize rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.Normalize cols) (I.Normalize rows))

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a              = KnownNat (I.Count a)
type CountableDimensions a b  = (Countable a, Countable b)
type CountableN a             = KnownNat (I.Count (I.Normalize a))
type CountableDimensionsN a b = (CountableN a, CountableN b)
type FromListsN e a b         = I.FromLists e (I.Normalize a) (I.Normalize b)
type Liftable a b             = (Eq b, Enumerable a, Enum a, EnumN a, Enum b, EnumN b, ConstructableN a b)
type Trivial a                = I.Normalize (I.Normalize a) ~ I.Normalize (I.Normalize (I.Normalize a))
type Trivial2 a               = I.Normalize a ~ I.Normalize (I.Normalize a)
type TrivialP a b             = I.Normalize (a, b) ~ I.Normalize (I.Normalize a, I.Normalize b)
type EqN a                    = Eq (I.Normalize a)
type EnumN a                  = Enum (I.Normalize a)
type Enumerable a             = (Enum a, Bounded a)
type EnumerableN a            = (Enum (I.Normalize a), Bounded (I.Normalize a))
type ConstructNorm a          = (Enum a, Enum (I.Normalize a))
type ConstructN a             = I.Construct (I.Normalize a)
type ConstructableN a b       = (I.Construct (I.Normalize a), I.Construct (I.Normalize b))

-- | It is possible to implement a constrained version of the category type
-- class.
instance (Num e) => Category (Matrix e) where
  type Object (Matrix e) a = (Num e, EqN a, ConstructN a, EnumerableN a)
  id = iden
  (.) = comp

-- | Bifunctor equivalent function
bimapM ::
       ( Num e,
         Liftable a b,
         Liftable c d
       ) => (a -> b) -> (c -> d) -> Matrix e a c -> Matrix e b d
bimapM f g m = fromF g . m . tr (fromF f)

-- | Zero type alias
type Zero = Void

-- | One type alias
type One = ()

-- Primitives

-- | Empty matrix constructor
empty :: Matrix e Zero Zero
empty = M I.Empty

-- | Unit matrix constructor
one :: e -> Matrix e One One
one = M . I.One

-- | Matrix 'Join' constructor
join ::
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
join (M a) (M b) = M (I.Join a b)

infixl 3 |||
-- | Matrix 'Join' constructor
(|||) ::
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
(|||) = join

-- | Matrix 'Fork' constructor
fork ::
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
fork (M a) (M b) = M (I.Fork a b)

infixl 2 ===
-- | Matrix 'Fork' constructor
(===) ::
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
(===) = fork

-- Functor hierarchy

-- | Functor instance equivalent function
fmapM :: (Num e, Liftable a b) => (a -> b) -> Matrix e c a -> Matrix e c b
fmapM f m = fromF f . m

-- | Applicative instance equivalent 'unit' function,
unitM :: Num e => Matrix e () ()
unitM = one 1

-- | Applicative instance equivalent 'unit' function,
multM :: (Num e, FstM a b, SndM a b) => Matrix e c a -> Matrix e c b -> Matrix e c (a, b)
multM = kr

-- | Monad instance equivalent 'return' function,
returnM :: (Num e, Liftable () a) => a -> Matrix e One a
returnM = point

-- | Monad instance equivalent '(>>=)' function,
bindM :: Num e => Matrix e a b -> Matrix e b c -> Matrix e a c
bindM = flip comp

-- Construction

-- | Build a matrix out of a list of list of elements. Throws a runtime
-- error if the dimensions do not match.
fromLists :: FromListsN e cols rows => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function that operates with arbitrary types.
matrixBuilder ::
              ( Num e,
                Enumerable a,
                Enum b,
                EnumN a,
                EnumN b,
                ConstructableN a b
              )
              => ((a, b) -> e) -> Matrix e a b
matrixBuilder = M . I.matrixBuilderN

-- | Constructs a column vector matrix
colL :: FromListsN e () rows => [e] -> Matrix e One rows
colL = M . I.colL

-- | Constructs a column vector matrix
col :: (Num e, Enum a, EnumN a, ConstructN a) => (a -> e) -> Matrix e One a
col = M . I.colN . I.Vector

-- | Constructs a row vector matrix
rowL :: FromListsN e cols () => [e] -> Matrix e cols One
rowL = M . I.rowL

-- | Constructs a row vector matrix
row :: (Num e, Enum a, EnumN a, ConstructN a) => (a -> e) -> Matrix e a One
row = M . I.rowN . I.Vector

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
fromF :: (Num e, Liftable a b)
      => (a -> b) -> Matrix e a b
fromF = M . I.fromFN

-- | Lifts relation functions to Boolean Matrix
toRel :: Liftable a b => (a -> b -> Bool) -> Matrix (Natural 0 1) a b
toRel = M . I.toRelN

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Matrix e cols rows -> [[e]]
toLists (M m) = I.toLists m

-- | Converts a matrix to a list of elements.
toList :: Matrix e cols rows -> [e]
toList (M m) = I.toList m

-- Zeros Matrix

-- | The zero matrix. A matrix wholly filled with zeros.
zeros ::
      ( Num e,
        ConstructableN cols rows,
        EnumerableN cols
      )
      => Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

-- | The ones matrix. A matrix wholly filled with ones.
--
--   Also known as T (Top) matrix.
ones ::
     ( Num e,
       ConstructableN cols rows,
       EnumerableN cols
     )
     => Matrix e cols rows
ones = M I.ones

-- Const Matrix

-- | The constant matrix constructor. A matrix wholly filled with a given
-- value.
constant ::
         ( Num e,
           ConstructableN cols rows,
           EnumerableN cols
         )
         => e -> Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

-- | The T (Top) row vector matrix.
bang ::
     ( Num e,
       ConstructN cols,
       EnumerableN cols
     )
     => Matrix e cols One
bang = ones

-- | Point constant matrix
point :: (Num e, Liftable () a) => a -> Matrix e One a
point = fromF . const

-- iden Matrix

-- | iden matrix
iden ::
     ( Num e,
       EqN a,
       ConstructN a,
       EnumerableN a
     )
     => Matrix e a a
iden = M I.iden
{-# NOINLINE iden #-}

-- Matrix composition (MMM)

-- | Matrix composition. Equivalent to matrix-matrix multiplication.
--
--   This definition takes advantage of divide-and-conquer and fusion laws
-- from LAoP.
comp :: Num e => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)
{-# NOINLINE comp #-}
{-# RULES 
   "comp/iden1" forall m. comp m iden = m ;
   "comp/iden2" forall m. comp iden m = m
#-}

-- Scalar multiplication of matrices

infixl 7 .|
-- | Scalar multiplication of matrices.
(.|) :: Num e => e -> Matrix e cols rows -> Matrix e cols rows
(.|) e (M m) = M (e I..| m)

-- Scalar division of matrices

infixl 7 ./
-- | Scalar multiplication of matrices.
(./) :: Fractional e => Matrix e cols rows -> e -> Matrix e cols rows
(./) (M m) e = M (m I../ e)

-- | Biproduct first component projection
p1 ::
   ( Num e,
     EqN m,
     ConstructableN m n,
     EnumerableN m,
     EnumerableN n
   )
   => Matrix e (Either m n) m
p1 = M I.p1

-- | Biproduct second component projection
p2 ::
   ( Num e,
     EqN n,
     ConstructableN m n,
     EnumerableN m,
     EnumerableN n
   )
   => Matrix e (Either m n) n
p2 = M I.p2

-- Injections

-- | Biproduct first component injection
i1 ::
   ( Num e,
     EqN m,
     ConstructableN m n,
     EnumerableN m,
     EnumerableN n
   )
   => Matrix e m (Either m n)
i1 = tr p1

-- | Biproduct second component injection
i2 ::
   ( Num e,
     EqN n,
     ConstructableN m n,
     EnumerableN m,
     EnumerableN n
   )
   => Matrix e n (Either m n)
i2 = tr p2

-- Dimensions

-- | Obtain the number of rows.
--
--   NOTE: The 'KnownNat' constaint is needed in order to obtain the
-- dimensions in constant time.
--
-- TODO: A 'rows' function that does not need the 'KnownNat' constraint in
-- exchange for performance.
rows :: CountableN rows => Matrix e cols rows -> Int
rows (M m) = I.rows m

-- | Obtain the number of columns.
-- 
--   NOTE: The 'KnownNat' constaint is needed in order to obtain the
-- dimensions in constant time.
--
-- TODO: A 'columns' function that does not need the 'KnownNat' constraint in
-- exchange for performance.
columns :: CountableN cols => Matrix e cols rows -> Int
columns (M m) = I.columns m

-- Coproduct Bifunctor

infixl 5 -|-

-- | Matrix coproduct functor also known as matrix direct sum.
(-|-) ::
      ( Num e,
        EqN k,
        EqN j,
        ConstructableN k j,
        EnumerableN k,
        EnumerableN j
      ) => Matrix e n k -> Matrix e m j -> Matrix e (Either n m) (Either k j)
(-|-) (M a) (M b) = M ((I.-|-) a b)

-- Khatri Rao Product and projections

type FstM a b =
  ( EqN a,
    ConstructableN (I.Normalize a) (I.Normalize (a, b)),
    ConstructN (I.Normalize a, I.Normalize b),
    EnumerableN a,
    EnumerableN b,
    EnumN (I.Normalize a, I.Normalize b),
    Trivial2 a,
    TrivialP a b
  )

-- | Khatri Rao product first component projection matrix.
fstM ::
     forall e a b .
     ( Num e,
       FstM a b
     ) => Matrix e (a, b) a
fstM = M (I.fstM @e @(I.Normalize a) @(I.Normalize b))

type SndM a b =
  ( EqN b,
    ConstructableN (I.Normalize b) (I.Normalize (a, b)),
    ConstructN (I.Normalize a, I.Normalize b),
    EnumerableN a,
    EnumerableN b,
    EnumN (I.Normalize a, I.Normalize b),
    Trivial2 b,
    TrivialP a b
  )
-- | Khatri Rao product second component projection matrix.
sndM ::
     forall e a b.
     ( Num e,
       SndM a b
     ) => Matrix e (a, b) b
sndM = M (I.sndM @e @(I.Normalize a) @(I.Normalize b))

-- | Khatri Rao Matrix product also known as matrix pairing.
--
--   NOTE: That this is not a true categorical product, see for instance:
-- 
-- @
--            | fstM . kr a b == a 
-- kr a b ==> |
--            | sndM . kr a b == b
-- @
--
-- __Emphasis__ on the implication symbol.
kr ::
   forall e cols a b.
   ( Num e,
     FstM a b,
     SndM a b
   )
   => Matrix e cols a -> Matrix e cols b -> Matrix e cols (a, b)
kr a b =
  let fstM' = fstM @e @a @b
      sndM' = sndM @e @a @b
   in comp (tr fstM') a * comp (tr sndM') b

-- Product Bifunctor (Kronecker)

infixl 4 ><
type Kronecker m n p q =
  ( EqN (I.Normalize m),
    EqN (I.Normalize n),
    EqN (I.Normalize p),
    EqN (I.Normalize q),
    ConstructableN (I.Normalize m) (I.Normalize n),
    ConstructableN (I.Normalize p) (I.Normalize q),
    ConstructableN (I.Normalize m, I.Normalize n) (I.Normalize p, I.Normalize q),
    ConstructableN (I.Normalize (I.Normalize m, I.Normalize n)) (I.Normalize (I.Normalize p, I.Normalize q)),
    FstM m n,
    SndM m n,
    EnumerableN (I.Normalize m),
    EnumerableN (I.Normalize n),
    EnumerableN (I.Normalize p),
    EnumerableN (I.Normalize q),
    EnumerableN (I.Normalize m, I.Normalize n),
    EnumerableN (I.Normalize p, I.Normalize q),
    Trivial2 m,
    Trivial2 n,
    Trivial2 p,
    Trivial2 q,
    TrivialP p q,
    TrivialP m m
  )

-- | Matrix product functor also known as Kronecker product
(><) ::
     forall e m p n q.
     ( Num e,
       Kronecker m n p q
     )
     => Matrix e m p -> Matrix e n q -> Matrix e (m, n) (p, q)
(><) a b =
  let fstM' = fstM @e @m @n
      sndM' = sndM @e @m @n
   in kr (comp a fstM') (comp b sndM')

-- Matrix abide Join Fork

-- | Matrix "abiding" followin the 'Join'-'Fork' abide law.
-- 
-- Law:
--
-- @
-- 'Join' ('Fork' a c) ('Fork' b d) == 'Fork' ('Join' a b) ('Join' c d)
-- @
abideJF :: Matrix e cols rows -> Matrix e cols rows
abideJF (M m) = M (I.abideJF m)

-- Matrix abide Fork Join

-- | Matrix "abiding" followin the 'Fork'-'Join' abide law.
-- 
-- Law:
--
-- @
-- 'Fork' ('Join' a b) ('Join' c d) == 'Join' ('Fork' a c) ('Fork' b d)
-- @
abideFJ :: Matrix e cols rows -> Matrix e cols rows
abideFJ (M m) = M (I.abideFJ m)

-- Matrix transposition

-- | Matrix transposition.
tr :: Matrix e cols rows -> Matrix e rows cols
tr (M m) = M (I.tr m)

-- Selective 'select' operator

-- | Selective functors 'select' operator equivalent inspired by the
-- ArrowMonad solution presented in the paper.
selectM :: (Num e, EqN b, ConstructN b, EnumerableN b) => Matrix e cols (Either a b) -> Matrix e a b -> Matrix e cols b
selectM (M m) (M y) = M (I.select m y)

-- McCarthy's Conditional

-- | McCarthy's Conditional expresses probabilistic choice.
-- cond ::
--      (
--      )
--      =>
--      (a -> Bool) -> Matrix e a b -> Matrix e a b -> Matrix e a b
-- cond p (M a) (M b) =

-- Pretty print

-- | Matrix pretty printer
pretty :: (Show e, CountableDimensionsN cols rows) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

-- | Matrix pretty printer
prettyPrint :: (CountableDimensionsN cols rows, Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e a b -> Matrix f a b -> Matrix g a b
zipWithM f (M a) (M b) = M (I.zipWithM f a b)
