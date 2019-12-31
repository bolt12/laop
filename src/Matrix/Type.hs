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
-- Module     : Matrix.Type
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

module Matrix.Type
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
    constant,

    -- * Misc
    -- ** Get dimensions
    columns,
    rows,

    -- ** Matrix Transposition
    tr,

    -- ** Selective operator
    select, 

    -- ** McCarthy's Conditional
    cond,

    -- ** Matrix "abiding"
    abideJS,
    abideSJ,

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
import qualified Control.Category as C
import qualified Matrix.Internal as I

newtype Matrix e (cols :: Type) (rows :: Type) = M (I.Matrix e (I.Normalize cols) (I.Normalize rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.Normalize cols) (I.Normalize rows))

-- | It isn't possible to implement the 'id' function so it's
-- implementation is 'undefined'. However 'comp' can be and this partial
-- class implementation exists just to make the code more readable.
--
-- Please use 'identity' instead.
instance (Num e) => C.Category (Matrix e) where
    id = undefined
    (.) = comp

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
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
junc (M a) (M b) = M (I.Junc a b)

infixl 3 |||
-- | Matrix 'Junc' constructor
(|||) ::
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (Either a b) rows
(|||) = junc

-- | Matrix 'Split' constructor
split ::
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
split (M a) (M b) = M (I.Split a b)

infixl 2 ===
-- | Matrix 'Split' constructor
(===) ::
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (Either a b)
(===) = split

-- Construction

-- | Build a matrix out of a list of list of elements. Throws a runtime
-- error if the dimensions do not match.
fromLists :: (I.FromLists e (I.Normalize cols) (I.Normalize rows)) => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function.
matrixBuilder ::
  (I.FromLists e (I.Normalize cols) (I.Normalize rows), KnownNat (I.Count (I.Normalize cols)), KnownNat (I.Count (I.Normalize rows))) =>
  ((Int, Int) -> e) ->
  Matrix e cols rows
matrixBuilder = M . I.matrixBuilder

-- | Constructs a column vector matrix
col :: (I.FromLists e () (I.Normalize rows)) => [e] -> Matrix e One rows
col = M . I.col

-- | Constructs a row vector matrix
row :: (I.FromLists e (I.Normalize cols) ()) => [e] -> Matrix e cols One
row = M . I.row

-- | Lifts functions to matrices with arbitrary dimensions.
--
--   NOTE: Be careful to not ask for a matrix bigger than the cardinality of
-- types @a@ or @b@ allows.
fromF ::
  ( Bounded a,
    Bounded b,
    Enum a,
    Enum b,
    Eq b,
    Num e,
    Ord e,
    KnownNat (I.Count (I.Normalize cols)),
    KnownNat (I.Count (I.Normalize rows)),
    I.FromLists e (I.Normalize rows) (I.Normalize cols)
  ) =>
  (a -> b) ->
  Matrix e cols rows
fromF = M . I.fromF

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
fromF' ::
  ( Bounded a,
    Bounded b,
    Enum a,
    Enum b,
    Eq b,
    Num e,
    Ord e,
    KnownNat (I.Count (I.Normalize a)),
    KnownNat (I.Count (I.Normalize b)),
    I.FromLists e (I.Normalize b) (I.Normalize a)
  ) =>
  (a -> b) ->
  Matrix e a b
fromF' = M . I.fromF'

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
  (Num e, I.FromLists e (I.Normalize cols) (I.Normalize rows), KnownNat (I.Count (I.Normalize cols)), KnownNat (I.Count (I.Normalize rows))) =>
  Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

-- | The ones matrix. A matrix wholly filled with ones.
--
--   Also known as T (Top) matrix.
ones ::
  (Num e, I.FromLists e (I.Normalize cols) (I.Normalize rows), KnownNat (I.Count (I.Normalize cols)), KnownNat (I.Count (I.Normalize rows))) =>
  Matrix e cols rows
ones = M I.ones

-- Const Matrix

-- | The constant matrix constructor. A matrix wholly filled with a given
-- value.
constant ::
  (Num e, I.FromLists e (I.Normalize cols) (I.Normalize rows), KnownNat (I.Count (I.Normalize cols)), KnownNat (I.Count (I.Normalize rows))) =>
  e ->
  Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

-- | The T (Top) row vector matrix.
bang ::
  forall e cols.
  (Num e, Enum e, I.FromLists e (I.Normalize cols) (), KnownNat (I.Count (I.Normalize cols))) =>
  Matrix e cols One
bang = M I.bang

-- Identity Matrix

-- | Identity matrix
identity ::
  (Num e, I.FromLists e (I.Normalize a) (I.Normalize a), KnownNat (I.Count (I.Normalize a))) =>
  Matrix e a a
identity = M I.identity

-- Matrix composition (MMM)

-- | Matrix composition. Equivalent to matrix-matrix multiplication.
--
--   This definition takes advantage of divide-and-conquer and fusion laws
-- from LAoP.
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)

-- | Biproduct first component projection
p1 ::
  ( Num e,
    KnownNat (I.Count (I.Normalize n)),
    KnownNat (I.Count (I.Normalize m)),
    I.FromLists e (I.Normalize n) (I.Normalize m),
    I.FromLists e (I.Normalize m) (I.Normalize m),
    I.Normalize (Either m n) ~ Either (I.Normalize m) (I.Normalize n)
  ) =>
  Matrix e (Either m n) m
p1 = M I.p1

-- | Biproduct second component projection
p2 ::
  ( Num e,
    KnownNat (I.Count (I.Normalize n)),
    KnownNat (I.Count (I.Normalize m)),
    I.FromLists e (I.Normalize m) (I.Normalize n),
    I.FromLists e (I.Normalize n) (I.Normalize n),
    I.Normalize (Either m n) ~ Either (I.Normalize m) (I.Normalize n)
  ) =>
  Matrix e (Either m n) n
p2 = M I.p2

-- Injections

-- | Biproduct first component injection
i1 ::
  ( Num e,
    KnownNat (I.Count (I.Normalize n)),
    KnownNat (I.Count (I.Normalize m)),
    I.FromLists e (I.Normalize n) (I.Normalize m),
    I.FromLists e (I.Normalize m) (I.Normalize m),
    I.Normalize (Either m n) ~ Either (I.Normalize m) (I.Normalize n)
  ) =>
  Matrix e m (Either m n)
i1 = tr p1

-- | Biproduct second component injection
i2 ::
  ( Num e,
    KnownNat (I.Count (I.Normalize n)),
    KnownNat (I.Count (I.Normalize m)),
    I.FromLists e (I.Normalize m) (I.Normalize n),
    I.FromLists e (I.Normalize n) (I.Normalize n),
    I.Normalize (Either m n) ~ Either (I.Normalize m) (I.Normalize n)
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
rows :: (KnownNat (I.Count (I.Normalize rows))) => Matrix e cols rows -> Int
rows (M m) = I.rows m

-- | Obtain the number of columns.
-- 
--   NOTE: The 'KnownNat' constaint is needed in order to obtain the
-- dimensions in constant time.
--
-- TODO: A 'columns' function that does not need the 'KnownNat' constraint in
-- exchange for performance.
columns :: (KnownNat (I.Count (I.Normalize cols))) => Matrix e cols rows -> Int
columns (M m) = I.columns m

-- Coproduct Bifunctor

infixl 5 -|-

-- | Matrix coproduct functor also known as matrix direct sum.
(-|-) ::
  ( Num e,
    KnownNat (I.Count (I.Normalize j)),
    KnownNat (I.Count (I.Normalize k)),
    I.FromLists e (I.Normalize k) (I.Normalize k),
    I.FromLists e (I.Normalize j) (I.Normalize k),
    I.FromLists e (I.Normalize k) (I.Normalize j),
    I.FromLists e (I.Normalize j) (I.Normalize j),
    I.Normalize (Either n m) ~ Either (I.Normalize n) (I.Normalize m),
    I.Normalize (Either k j) ~ Either (I.Normalize k) (I.Normalize j)
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
    KnownNat (I.Count (I.Normalize m)),
    KnownNat (I.Count (I.Normalize k)),
    KnownNat (I.Count (I.Normalize (m, k))),
    I.FromLists e (I.Normalize (m, k)) (I.Normalize m),
    I.Normalize (m, k) ~ I.Normalize (I.Normalize m, I.Normalize k)
  ) => Matrix e (m, k) m
kp1 = M (I.kp1 @e @(I.Normalize m) @(I.Normalize k))

-- | Khatri Rao product second component projection matrix.
kp2 :: 
    forall e m k.
    ( Num e,
      KnownNat (I.Count (I.Normalize k)),
      KnownNat (I.Count (I.Normalize m)),
      KnownNat (I.Count (I.Normalize (m, k))),
      I.FromLists e (I.Normalize (m, k)) (I.Normalize k),
      I.Normalize (m, k) ~ I.Normalize (I.Normalize m, I.Normalize k)
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
    KnownNat (I.Count (I.Normalize a)),
    KnownNat (I.Count (I.Normalize b)),
    KnownNat (I.Count (I.Normalize (a, b))),
    I.FromLists e (I.Normalize (a, b)) (I.Normalize a),
    I.FromLists e (I.Normalize (a, b)) (I.Normalize b),
    I.Normalize (a, b) ~ I.Normalize (I.Normalize a, I.Normalize b)
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
    KnownNat (I.Count (I.Normalize m)),
    KnownNat (I.Count (I.Normalize n)),
    KnownNat (I.Count (I.Normalize p)),
    KnownNat (I.Count (I.Normalize q)),
    KnownNat (I.Count (I.Normalize (m, n))),
    KnownNat (I.Count (I.Normalize (p, q))),
    I.FromLists e (I.Normalize (m, n)) (I.Normalize m),
    I.FromLists e (I.Normalize (m, n)) (I.Normalize n),
    I.FromLists e (I.Normalize (p, q)) (I.Normalize p),
    I.FromLists e (I.Normalize (p, q)) (I.Normalize q),
    I.Normalize (m, n) ~ I.Normalize (I.Normalize m, I.Normalize n),
    I.Normalize (p, q) ~ I.Normalize (I.Normalize p, I.Normalize q)
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
select :: 
       ( Bounded a,
         Bounded b,
         Enum a,
         Eq b,
         Enum b,
         Num e,
         Ord e,
         KnownNat (I.Count (I.Normalize a)),
         KnownNat (I.Count (I.Normalize b)),
         KnownNat (I.Count (I.Normalize cols)),
         I.FromLists e (I.Normalize b) (I.Normalize a),
         I.FromLists e (I.Normalize b) (I.Normalize b),
         I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)
       ) 
       => Matrix e cols (Either a b) -> (a -> b) -> Matrix e cols b
select (M m) y = M (I.select m y)

-- McCarthy's Conditional

-- | McCarthy's Conditional expresses probabilistic choice.
cond ::
     ( I.Normalize (I.Normalize a) ~ I.Normalize a,
       KnownNat (I.Count (I.Normalize a)),
       I.FromLists e () (I.Normalize a),
       I.FromLists e (I.Normalize a) (),
       I.FromLists e (I.Normalize a) (I.Normalize a),
       Bounded a,
       Enum a,
       Num e,
       Ord e
     )
     =>
     (a -> Bool) -> Matrix e a b -> Matrix e a b -> Matrix e a b
cond p (M a) (M b) = M (I.cond p a b)

-- Pretty print

-- | Matrix pretty printer
pretty :: (KnownNat (I.Count (I.Normalize cols)), Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

-- | Matrix pretty printer
prettyPrint :: (KnownNat (I.Count (I.Normalize cols)), Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m
