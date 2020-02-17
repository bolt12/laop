{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    TrivialP,

    -- * Type aliases
    Zero,
    One,

    -- * Primitives
    empty,
    one,
    junc,
    split,

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
    rows,

    -- ** Matrix Transposition
    tr,

    -- ** McCarthy's Conditional
    cond,

    -- ** Matrix "abiding"
    abideJS,
    abideSJ,

    -- ** Zip Matrices
    zipWithM,

    -- * Biproduct approach
    -- ** Split
    (===),
    -- *** Projections
    p1,
    p2,
    -- ** Junc
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
    kp1,
    kp2,

    -- *** Matrix pairing
    khatri,

    -- * Matrix composition and lifting

    -- ** Arrow matrix combinators

    -- | Note that given the restrictions imposed it is not possible to
    -- implement the standard type classes present in standard Haskell.
    identity,
    comp,
    fromF,
    fromF',

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
import GHC.TypeLits 
import Control.DeepSeq
import LAoP.Utils
import qualified Control.Category as C
import qualified LAoP.Matrix.Internal as I

newtype Matrix e (cols :: Type) (rows :: Type) = M (I.Matrix e (I.Normalize cols) (I.Normalize rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.Normalize cols) (I.Normalize rows))

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a              = KnownNat (I.Count a)
type CountableDimensions a b  = (Countable a, Countable b)
type CountableN a             = KnownNat (I.Count (I.Normalize a))
type CountableDimensionsN a b = (CountableN a, CountableN b)
type FromListsN e a b         = I.FromLists e (I.Normalize a) (I.Normalize b)
type Liftable e a b           = (Bounded a, Bounded b, Enum a, Enum b, Eq b, Num e, Ord e)
type Trivial a                = I.Normalize (I.Normalize a) ~ I.Normalize (I.Normalize (I.Normalize a))
type Trivial2 a               = I.Normalize a ~ I.Normalize (I.Normalize a)
type Trivial3 a               = I.FromNat (I.Count (I.Normalize (I.Normalize a))) ~ I.Normalize (I.Normalize a)
type TrivialP a b             = I.Normalize (a, b) ~ I.Normalize (I.Normalize a, I.Normalize b)

-- | It isn't possible to implement the 'id' function so it's
-- implementation is 'undefined'. However 'comp' can be and this partial
-- class implementation exists just to make the code more readable.
--
-- Please use 'identity' instead.
instance (Num e) => C.Category (Matrix e) where
    id = undefined
    (.) = comp

-- | Bifunctor equivalent function
bimapM ::
       ( Liftable e a b,
         Liftable e c d,
         CountableDimensionsN a c,
         CountableDimensionsN b d,
         FromListsN e d c,
         FromListsN e b a
       ) => (a -> b) -> (c -> d) -> Matrix e a c -> Matrix e b d
bimapM f g m = fromF' g `comp` m `comp` tr (fromF' f)

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

-- | Matrix 'Junc' constructor
junc ::
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
junc (M a) (M b) = M (I.Junc a b)

infixl 3 |||
-- | Matrix 'Junc' constructor
(|||) ::
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
(|||) = junc

-- | Matrix 'Split' constructor
split ::
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
split (M a) (M b) = M (I.Split a b)

infixl 2 ===
-- | Matrix 'Split' constructor
(===) ::
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
(===) = split

-- Functor hierarchy

-- | Functor instance equivalent function
fmapM :: 
     ( Liftable e a b,
       CountableDimensionsN a b,
       FromListsN e b a
     )
     =>
     (a -> b) -> Matrix e c a -> Matrix e c b
fmapM f m = fromF' f `comp` m

-- | Applicative instance equivalent 'unit' function,
unitM :: (Num e) => Matrix e () ()
unitM = one 1

-- | Applicative instance equivalent 'unit' function,
multM :: 
      ( CountableDimensionsN a b,
        CountableN (a, b),
        Num e,
        FromListsN e (a, b) a,
        FromListsN e (a, b) b,
        TrivialP a b
      ) => Matrix e c a -> Matrix e c b -> Matrix e c (a, b)
multM = khatri

-- | Monad instance equivalent 'return' function,
returnM :: 
        forall e a . 
        ( Num e,
          Enum e,
          Enum a,
          FromListsN e () a,
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
fromLists :: (FromListsN e cols rows) => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function.
matrixBuilder ::
  (FromListsN e cols rows, CountableDimensionsN cols rows )
  => ((Int, Int) -> e) -> Matrix e cols rows
matrixBuilder = M . I.matrixBuilder

-- | Constructs a column vector matrix
col :: (FromListsN e () rows) => [e] -> Matrix e One rows
col = M . I.col

-- | Constructs a row vector matrix
row :: (FromListsN e cols ()) => [e] -> Matrix e cols One
row = M . I.row

-- | Lifts functions to matrices with arbitrary dimensions.
--
--   NOTE: Be careful to not ask for a matrix bigger than the cardinality of
-- types @a@ or @b@ allows.
fromF ::
  ( Liftable e a b,
    CountableDimensionsN cols rows,
    FromListsN e rows cols
  ) =>
  (a -> b) -> Matrix e cols rows
fromF = M . I.fromF

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
fromF' ::
  ( Liftable e a b,
    CountableDimensionsN a b,
    FromListsN e b a
  ) =>
  (a -> b) -> Matrix e a b
fromF' = M . I.fromF'

-- | Lifts relation functions to Boolean Matrix
toRel ::
  ( Liftable (Natural 0 1) a b,
    CountableDimensionsN a b,
    FromListsN (Natural 0 1) b a
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
  (Num e, FromListsN e cols rows, CountableDimensionsN cols rows) 
  => Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

-- | The ones matrix. A matrix wholly filled with ones.
--
--   Also known as T (Top) matrix.
ones ::
  (Num e, FromListsN e cols rows, CountableDimensionsN cols rows) 
  => Matrix e cols rows
ones = M I.ones

-- Const Matrix

-- | The constant matrix constructor. A matrix wholly filled with a given
-- value.
constant ::
  (Num e, FromListsN e cols rows, CountableDimensionsN cols rows) 
  => e -> Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

-- | The T (Top) row vector matrix.
bang ::
  forall e cols.
  (Num e, Enum e, FromListsN e cols (), CountableN cols) =>
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
        FromListsN e a One
      ) => a -> Matrix e One a
point = fromF' . const

-- Identity Matrix

-- | Identity matrix
identity ::
  (Num e, FromListsN e a a, CountableN a) =>
  Matrix e a a
identity = M I.identity
{-# NOINLINE identity #-}

-- Matrix composition (MMM)

-- | Matrix composition. Equivalent to matrix-matrix multiplication.
--
--   This definition takes advantage of divide-and-conquer and fusion laws
-- from LAoP.
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)
{-# NOINLINE comp #-}
{-# RULES 
   "comp/identity1" forall m. comp m identity = m ;
   "comp/identity2" forall m. comp identity m = m
#-}

-- | Biproduct first component projection
p1 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e n m,
    FromListsN e m m
  ) =>
  Matrix e (Either m n) m
p1 = M I.p1

-- | Biproduct second component projection
p2 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e m n,
    FromListsN e n n
  ) =>
  Matrix e (Either m n) n
p2 = M I.p2

-- Injections

-- | Biproduct first component injection
i1 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e n m,
    FromListsN e m m
  ) =>
  Matrix e m (Either m n)
i1 = tr p1

-- | Biproduct second component injection
i2 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e m n,
    FromListsN e n n
  ) =>
  Matrix e n (Either m n)
i2 = tr p2

-- Dimensions

-- | Obtain the number of rows.
--
--   NOTE: The 'KnownNat' constaint is needed in order to obtain the
-- dimensions in constant time.
--
-- TODO: A 'rows' function that does not need the 'KnownNat' constraint in
-- exchange for performance.
rows :: (CountableN rows) => Matrix e cols rows -> Int
rows (M m) = I.rows m

-- | Obtain the number of columns.
-- 
--   NOTE: The 'KnownNat' constaint is needed in order to obtain the
-- dimensions in constant time.
--
-- TODO: A 'columns' function that does not need the 'KnownNat' constraint in
-- exchange for performance.
columns :: (CountableN cols) => Matrix e cols rows -> Int
columns (M m) = I.columns m

-- Coproduct Bifunctor

infixl 5 -|-

-- | Matrix coproduct functor also known as matrix direct sum.
(-|-) ::
  ( Num e,
    CountableDimensionsN j k,
    FromListsN e k k,
    FromListsN e j k,
    FromListsN e k j,
    FromListsN e j j
  ) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (Either n m) (Either k j)
(-|-) (M a) (M b) = M ((I.-|-) a b)

-- Khatri Rao Product and projections

-- | Khatri Rao product first component projection matrix.
kp1 :: 
  forall e m k .
  ( Num e,
    CountableDimensionsN m k,
    CountableN (m, k),
    FromListsN e (m, k) m,
    TrivialP m k
  ) => Matrix e (m, k) m
kp1 = M (I.kp1 @e @(I.Normalize m) @(I.Normalize k))

-- | Khatri Rao product second component projection matrix.
kp2 :: 
    forall e m k.
    ( Num e,
      CountableDimensionsN k m,
      CountableN (m, k),
      FromListsN e (m, k) k,
      TrivialP m k
    ) => Matrix e (m, k) k
kp2 = M (I.kp2 @e @(I.Normalize m) @(I.Normalize k))

-- | Khatri Rao Matrix product also known as matrix pairing.
--
--   NOTE: That this is not a true categorical product, see for instance:
-- 
-- @
--                | kp1 `comp` khatri a b == a 
-- khatri a b ==> |
--                | kp2 `comp` khatri a b == b
-- @
--
-- __Emphasis__ on the implication symbol.
khatri ::
  forall e cols a b.
  ( Num e,
    CountableDimensionsN a b,
    CountableN (a, b),
    FromListsN e (a, b) a,
    FromListsN e (a, b) b,
    TrivialP a b
  ) => Matrix e cols a -> Matrix e cols b -> Matrix e cols (a, b)
khatri a b =
  let kp1' = kp1 @e @a @b
      kp2' = kp2 @e @a @b
   in comp (tr kp1') a * comp (tr kp2') b

-- Product Bifunctor (Kronecker)

infixl 4 ><

-- | Matrix product functor also known as kronecker product
(><) ::
  forall e m p n q.
  ( Num e,
    CountableDimensionsN m n,
    CountableDimensionsN p q,
    CountableDimensionsN (m, n) (p, q),
    FromListsN e (m, n) m,
    FromListsN e (m, n) n,
    FromListsN e (p, q) p,
    FromListsN e (p, q) q,
    TrivialP m n,
    TrivialP p q
  ) => Matrix e m p -> Matrix e n q -> Matrix e (m, n) (p, q)
(><) a b =
  let kp1' = kp1 @e @m @n
      kp2' = kp2 @e @m @n
   in khatri (comp a kp1') (comp b kp2')

-- Matrix abide Junc Split

-- | Matrix "abiding" followin the 'Junc'-'Split' abide law.
-- 
-- Law:
--
-- @
-- 'Junc' ('Split' a c) ('Split' b d) == 'Split' ('Junc' a b) ('Junc' c d)
-- @
abideJS :: Matrix e cols rows -> Matrix e cols rows
abideJS (M m) = M (I.abideJS m)

-- Matrix abide Split Junc

-- | Matrix "abiding" followin the 'Split'-'Junc' abide law.
-- 
-- Law:
--
-- @
-- 'Split' ('Junc' a b) ('Junc' c d) == 'Junc' ('Split' a c) ('Split' b d)
-- @
abideSJ :: Matrix e cols rows -> Matrix e cols rows
abideSJ (M m) = M (I.abideSJ m)

-- Matrix transposition

-- | Matrix transposition.
tr :: Matrix e cols rows -> Matrix e rows cols
tr (M m) = M (I.tr m)

-- Selective 'select' operator

-- | Selective functors 'select' operator equivalent inspired by the
-- ArrowMonad solution presented in the paper.
selectM :: 
       ( Num e,
         FromListsN e b b,
         CountableN b
       ) => Matrix e cols (Either a b) -> Matrix e a b -> Matrix e cols b
selectM (M m) (M y) = M (I.select m y)

-- McCarthy's Conditional

-- | McCarthy's Conditional expresses probabilistic choice.
cond ::
     ( Trivial a,
       Trivial2 a,
       Trivial3 a,
       CountableN a,
       FromListsN e () a,
       FromListsN e a (),
       FromListsN e a a,
       Liftable e a Bool
     )
     =>
     (a -> Bool) -> Matrix e a b -> Matrix e a b -> Matrix e a b
cond p (M a) (M b) = M (I.cond p a b)

-- Pretty print

-- | Matrix pretty printer
pretty :: (CountableDimensionsN cols rows, Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

-- | Matrix pretty printer
prettyPrint :: (CountableDimensionsN cols rows, Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e a b -> Matrix f a b -> Matrix g a b
zipWithM f (M a) (M b) = M (I.zipWithM f a b)
