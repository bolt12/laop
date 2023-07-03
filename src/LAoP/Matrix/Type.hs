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
    CountableDims,
    CountableN,
    CountableDimsN,
    FLN,
    Liftable,
    Trivial,
    TrivialP,

    -- * Type aliases
    Zero,
    One,

    -- * Primitives
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
    matrixBuilder',
    matrixBuilder,
    row,
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
    columns',
    rows,
    rows',

    -- ** Matrix Transposition
    tr,

    -- ** Scalar multiplication/division of matrices
    (.|),
    (./),

    -- ** McCarthy's Conditional
    cond,

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
    fromF',
    fromF,

    -- * Relation
    toRel,

    -- * Matrix printing
    pretty,
    prettyPrint
  )
where

import Data.Void
import Data.Proxy
import Data.Kind
import GHC.TypeLits hiding (Natural)
import Control.DeepSeq
import LAoP.Utils
import qualified LAoP.Matrix.Internal as I
import Prelude hiding (id, (.))

newtype Matrix e (cols :: Type) (rows :: Type) = M (I.Matrix e (I.Normalize cols) (I.Normalize rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.Normalize cols) (I.Normalize rows))

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a        = KnownNat (I.Count a)
type CountableDims a b  = (Countable a, Countable b)
type CountableN a       = KnownNat (I.Count (I.Normalize a))
type CountableDimsN a b = (CountableN a, CountableN b)
type FLN a b            = I.FromLists (I.Normalize a) (I.Normalize b)
type Liftable e a b     = (Bounded a, Bounded b, Enum a, Enum b, Eq b, Num e, Ord e)
type Trivial a          = I.Normalize (I.Normalize a) ~ I.Normalize (I.Normalize (I.Normalize a))
type Trivial2 a         = I.Normalize a ~ I.Normalize (I.Normalize a)
type Trivial3 a         = I.FromNat (I.Count (I.Normalize (I.Normalize a))) ~ I.Normalize (I.Normalize a)
type TrivialP a b       = I.Normalize (a, b) ~ I.Normalize (I.Normalize a, I.Normalize b)

-- | It is possible to implement a constrained version of the category type
-- class.
instance (Num e) => Category (Matrix e) where
  type Object (Matrix e) a = (FLN a a, CountableN a)
  id = iden
  (.) = comp

-- | Bifunctor equivalent function
bimapM ::
       ( Liftable e a b,
         Liftable e c d,
         CountableDimsN a c,
         CountableDimsN b d,
         FLN d c,
         FLN b a
       ) => (a -> b) -> (c -> d) -> Matrix e a c -> Matrix e b d
bimapM f g m = fromF g . m . tr (fromF f)

-- | Zero type alias
type Zero = Void

-- | One type alias
type One = ()

-- Primitives

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
fmapM ::
     ( Liftable e a b,
       CountableDimsN a b,
       FLN b a
     )
     =>
     (a -> b) -> Matrix e c a -> Matrix e c b
fmapM f m = fromF f . m

-- | Applicative instance equivalent 'unit' function,
unitM :: (Num e) => Matrix e () ()
unitM = one 1

-- | Applicative instance equivalent 'unit' function,
multM ::
      ( CountableDimsN a b,
        CountableN (a, b),
        Num e,
        FLN (a, b) a,
        FLN (a, b) b,
        TrivialP a b
      ) => Matrix e c a -> Matrix e c b -> Matrix e c (a, b)
multM = kr

-- | Monad instance equivalent 'return' function,
returnM ::
        forall e a .
        ( Num e,
          Enum e,
          Enum a,
          FLN () a,
          Countable a
        ) => a -> Matrix e One a
returnM a = col l
    where
        i = fromInteger $ natVal (Proxy :: Proxy (I.Count a))
        x = fromEnum a
        l = take x [0,0..] ++ [1] ++ take (i - x - 1) [0,0..]

-- | Monad instance equivalent '(>>=)' function,
bindM :: (Num e) => Matrix e a b -> Matrix e b c -> Matrix e a c
bindM = flip comp

-- Construction

-- | Build a matrix out of a list of list of elements. Throws a runtime
-- error if the dimensions do not match.
fromLists :: (FLN cols rows) => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function that operates with indices.
matrixBuilder' ::
  (FLN cols rows, CountableDimsN cols rows )
  => ((Int, Int) -> e) -> Matrix e cols rows
matrixBuilder' = M . I.matrixBuilder'

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function that operates with arbitrary types.
matrixBuilder ::
  ( FLN a b,
    Enum a,
    Enum b,
    Bounded a,
    Bounded b,
    Countable b
  ) => ((a, b) -> e) -> Matrix e a b
matrixBuilder f = M (I.matrixBuilder f)

-- | Constructs a column vector matrix
col :: (FLN () rows) => [e] -> Matrix e One rows
col = M . I.col

-- | Constructs a row vector matrix
row :: (FLN cols ()) => [e] -> Matrix e cols One
row = M . I.row

-- | Lifts functions to matrices with arbitrary dimensions.
--
--   NOTE: Be careful to not ask for a matrix bigger than the cardinality of
-- types @a@ or @b@ allows.
fromF' ::
  ( Liftable e a b,
    CountableDimsN cols rows,
    FLN rows cols
  ) =>
  (a -> b) -> Matrix e cols rows
fromF' = M . I.fromF'

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
fromF ::
  ( Liftable e a b,
    CountableDimsN a b,
    FLN b a
  ) =>
  (a -> b) -> Matrix e a b
fromF = M . I.fromF

-- | Lifts relation functions to Boolean Matrix
toRel ::
  ( Liftable (Natural 0 1) a b,
    CountableDims a b,
    FLN b a
  ) => (a -> b -> Bool) -> Matrix (Natural 0 1) a b
toRel = M . I.toRel

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
  (Num e, FLN cols rows, CountableDimsN cols rows)
  => Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

-- | The ones matrix. A matrix wholly filled with ones.
--
--   Also known as T (Top) matrix.
ones ::
  (Num e, FLN cols rows, CountableDimsN cols rows)
  => Matrix e cols rows
ones = M I.ones

-- Const Matrix

-- | The constant matrix constructor. A matrix wholly filled with a given
-- value.
constant ::
  (FLN cols rows, CountableDimsN cols rows)
  => e -> Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

-- | The T (Top) row vector matrix.
bang ::
  forall e cols.
  (Num e, Enum e, FLN cols (), CountableN cols) =>
  Matrix e cols One
bang = M I.bang

-- | Point constant relation
point ::
      ( Bounded a,
        Enum a,
        Eq a,
        Num e,
        Ord e,
        CountableN a,
        FLN a One
      ) => a -> Matrix e One a
point = fromF . const

-- iden Matrix

-- | iden matrix
iden ::
  (Num e, FLN a a, CountableN a) =>
  Matrix e a a
iden = M I.iden
{-# NOINLINE iden #-}

-- Matrix composition (MMM)

-- | Matrix composition. Equivalent to matrix-matrix multiplication.
--
--   This definition takes advantage of divide-and-conquer and fusion laws
-- from LAoP.
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
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
    CountableDimsN n m,
    FLN n m,
    FLN m m
  ) =>
  Matrix e (Either m n) m
p1 = M I.p1

-- | Biproduct second component projection
p2 ::
  ( Num e,
    CountableDimsN n m,
    FLN m n,
    FLN n n
  ) =>
  Matrix e (Either m n) n
p2 = M I.p2

-- Injections

-- | Biproduct first component injection
i1 ::
  ( Num e,
    CountableDimsN n m,
    FLN n m,
    FLN m m
  ) =>
  Matrix e m (Either m n)
i1 = tr p1

-- | Biproduct second component injection
i2 ::
  ( Num e,
    CountableDimsN n m,
    FLN m n,
    FLN n n
  ) =>
  Matrix e n (Either m n)
i2 = tr p2

-- Dimensions

-- | Obtain the number of rows.
--
--   NOTE: The 'KnownNat' constraint is needed in order to obtain the
-- dimensions in constant time. For a version that doesn't require the
-- constraint see 'rows''.
rows :: (CountableN rows) => Matrix e cols rows -> Int
rows (M m) = I.rows m

-- | Obtain the number of rows in an inefficient manner, but without any
-- constraints.
--
-- For a more efficient version see 'rows'.
rows' :: Matrix e cols rows -> Int
rows' (M m) = I.rows' m

-- | Obtain the number of columns.
--
--   NOTE: The 'KnownNat' constraint is needed in order to obtain the
-- dimensions in constant time. For a version that doesn't require the
-- constraint see 'columns''.
columns :: (CountableN cols) => Matrix e cols rows -> Int
columns (M m) = I.columns m

-- | Obtain the number of columns in an inefficient manner, but without any
-- constraints.
--
-- For a more efficient version see 'columns'.
columns' :: Matrix e cols rows -> Int
columns' (M m) = I.columns' m

-- Coproduct Bifunctor

infixl 5 -|-

-- | Matrix coproduct functor also known as matrix direct sum.
(-|-) ::
  ( Num e,
    CountableDimsN j k,
    FLN k k,
    FLN j k,
    FLN k j,
    FLN j j
  ) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (Either n m) (Either k j)
(-|-) (M a) (M b) = M ((I.-|-) a b)

-- Khatri Rao Product and projections

-- | Khatri Rao product first component projection matrix.
fstM ::
  forall e m k .
  ( Num e,
    CountableDimsN m k,
    CountableN (m, k),
    FLN (m, k) m,
    TrivialP m k
  ) => Matrix e (m, k) m
fstM = M (I.fstM @e @(I.Normalize m) @(I.Normalize k))

-- | Khatri Rao product second component projection matrix.
sndM ::
    forall e m k.
    ( Num e,
      CountableDimsN k m,
      CountableN (m, k),
      FLN (m, k) k,
      TrivialP m k
    ) => Matrix e (m, k) k
sndM = M (I.sndM @e @(I.Normalize m) @(I.Normalize k))

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
    CountableDimsN a b,
    CountableN (a, b),
    FLN (a, b) a,
    FLN (a, b) b,
    TrivialP a b
  ) => Matrix e cols a -> Matrix e cols b -> Matrix e cols (a, b)
kr a b =
  let fstM' = fstM @e @a @b
      sndM' = sndM @e @a @b
   in comp (tr fstM') a * comp (tr sndM') b

-- Product Bifunctor (Kronecker)

infixl 4 ><

-- | Matrix product functor also known as Kronecker product
(><) ::
  forall e m p n q.
  ( Num e,
    CountableDimsN m n,
    CountableDimsN p q,
    CountableDimsN (m, n) (p, q),
    FLN (m, n) m,
    FLN (m, n) n,
    FLN (p, q) p,
    FLN (p, q) q,
    TrivialP m n,
    TrivialP p q
  ) => Matrix e m p -> Matrix e n q -> Matrix e (m, n) (p, q)
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
selectM ::
       ( Num e,
         FLN b b,
         CountableN b
       ) => Matrix e cols (Either a b) -> Matrix e a b -> Matrix e cols b
selectM (M m) (M y) = M (I.select m y)

-- McCarthy's Conditional

-- | McCarthy's Conditional expresses probabilistic choice.
cond ::
     ( Trivial2 a,
       Trivial3 a,
       CountableN a,
       FLN () a,
       FLN a (),
       FLN a a,
       Liftable e a Bool
     )
     =>
     (a -> Bool) -> Matrix e a b -> Matrix e a b -> Matrix e a b
cond p (M a) (M b) = M (I.cond p a b)

-- Pretty print

-- | Matrix pretty printer
pretty :: (CountableDimsN cols rows, Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

-- | Matrix pretty printer
prettyPrint :: (CountableDimsN cols rows, Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e a b -> Matrix f a b -> Matrix g a b
zipWithM f (M a) (M b) = M (I.zipWithM f a b)
