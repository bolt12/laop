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
-- Module     : Matrix.Nat
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
-- uses type level naturals instead of standard data types for the matrices
-- dimensions.
--
-----------------------------------------------------------------------------

module Matrix.Nat
  ( -- | LAoP (Linear Algebra of Programming) Inductive Matrix definition.
    --
    --         LAoP generalises relations and functions treating them as
    --         Boolean matrices and in turn consider these as arrows.
    --         This library offers many of the combinators mentioned in the work of
    --         Macedo (2012) and Oliveira (2012).
    --
    --         This definition is a wrapper around 'Matrix.Type' but
    --         dimensions are type level Naturals. Type inference might not
    --         be as desired.
    --
    --         There exists two type families that make it easier to write
    --         matrix dimensions: 'FromNat' and 'Count'. This approach
    --         leads to a very straightforward implementation 
    --         of LAoP combinators. 

    -- * Type safe matrix representation
    Matrix (..),

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

import Data.Proxy
import GHC.TypeLits
import Control.DeepSeq
import qualified Control.Category as C
import qualified Matrix.Internal as I

newtype Matrix e (cols :: Nat) (rows :: Nat) = M (I.Matrix e (I.FromNat cols) (I.FromNat rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.FromNat cols) (I.FromNat rows))

-- | It isn't possible to implement the 'id' function so it's
-- implementation is 'undefined'. However 'comp' can be and this partial
-- class implementation exists just to make the code more readable.
--
-- Please use 'identity' instead.
instance (Num e) => C.Category (Matrix e) where
    id = undefined
    (.) = comp

-- Primitives

empty :: Matrix e 0 0
empty = M I.Empty

one :: e -> Matrix e 1 1
one = M . I.One

junc ::
  (I.FromNat cols3 ~ Either (I.FromNat cols1) (I.FromNat cols2)) =>
  Matrix e cols1 rows ->
  Matrix e cols2 rows ->
  Matrix e cols3 rows
junc (M a) (M b) = M (I.Junc a b)

infixl 3 |||
(|||) ::
  (I.FromNat cols3 ~ Either (I.FromNat cols1) (I.FromNat cols2)) =>
  Matrix e cols1 rows ->
  Matrix e cols2 rows ->
  Matrix e cols3 rows
(|||) = junc

split ::
  (I.FromNat rows3 ~ Either (I.FromNat rows1) (I.FromNat rows2)) =>
  Matrix e cols rows1 ->
  Matrix e cols rows2 ->
  Matrix e cols rows3
split (M a) (M b) = M (I.Split a b)

infixl 2 ===
(===) ::
  (I.FromNat rows3 ~ Either (I.FromNat rows1) (I.FromNat rows2)) =>
  Matrix e cols rows1 ->
  Matrix e cols rows2 ->
  Matrix e cols rows3
(===) = split

-- Construction

fromLists :: (I.FromLists e (I.FromNat cols) (I.FromNat rows)) => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

matrixBuilder ::
  (I.FromLists e (I.FromNat cols) (I.FromNat rows), KnownNat (I.Count (I.FromNat cols)), KnownNat (I.Count (I.FromNat rows))) =>
  ((Int, Int) -> e) ->
  Matrix e cols rows
matrixBuilder = M . I.matrixBuilder

col :: (I.FromLists e () (I.FromNat rows)) => [e] -> Matrix e 1 rows
col = M . I.col

row :: (I.FromLists e (I.FromNat cols) ()) => [e] -> Matrix e cols 1
row = M . I.row

fromF ::
  ( Bounded a,
    Bounded b,
    Enum a,
    Enum b,
    Eq b,
    Num e,
    Ord e,
    KnownNat (I.Count (I.FromNat cols)),
    KnownNat (I.Count (I.FromNat rows)),
    I.FromLists e (I.FromNat rows) (I.FromNat cols)
  ) =>
  (a -> b) ->
  Matrix e cols rows
fromF = M . I.fromF

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
  Matrix e (I.Count a) (I.Count b)
fromF' = M . I.fromF'

-- Conversion

toLists :: Matrix e cols rows -> [[e]]
toLists (M m) = I.toLists m

toList :: Matrix e cols rows -> [e]
toList (M m) = I.toList m

-- Zeros Matrix

zeros ::
  (Num e, I.FromLists e (I.FromNat cols) (I.FromNat rows), KnownNat (I.Count (I.FromNat cols)), KnownNat (I.Count (I.FromNat rows))) =>
  Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

ones ::
  (Num e, I.FromLists e (I.FromNat cols) (I.FromNat rows), KnownNat (I.Count (I.FromNat cols)), KnownNat (I.Count (I.FromNat rows))) =>
  Matrix e cols rows
ones = M I.ones

-- Const Matrix

constant ::
  (Num e, I.FromLists e (I.FromNat cols) (I.FromNat rows), KnownNat (I.Count (I.FromNat cols)), KnownNat (I.Count (I.FromNat rows))) =>
  e ->
  Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

bang ::
  forall e cols.
  (Num e, Enum e, I.FromLists e (I.FromNat cols) (), KnownNat (I.Count (I.FromNat cols))) =>
  Matrix e cols 1
bang = M I.bang

-- Identity Matrix

identity ::
  (Num e, I.FromLists e (I.FromNat cols) (I.FromNat cols), KnownNat (I.Count (I.FromNat cols))) =>
  Matrix e cols cols
identity = M I.identity

-- Matrix composition (MMM)

comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)

p1 ::
  ( Num e,
    KnownNat (I.Count (I.FromNat n)),
    KnownNat (I.Count (I.FromNat m)),
    I.FromLists e (I.FromNat n) (I.FromNat m),
    I.FromLists e (I.FromNat m) (I.FromNat m),
    I.FromNat (m + n) ~ Either (I.FromNat m) (I.FromNat n)
  ) =>
  Matrix e (m + n) m
p1 = M I.p1

p2 ::
  ( Num e,
    KnownNat (I.Count (I.FromNat n)),
    KnownNat (I.Count (I.FromNat m)),
    I.FromLists e (I.FromNat m) (I.FromNat n),
    I.FromLists e (I.FromNat n) (I.FromNat n),
    I.FromNat (m + n) ~ Either (I.FromNat m) (I.FromNat n)
  ) =>
  Matrix e (m + n) n
p2 = M I.p2

-- Injections

i1 ::
  ( Num e,
    KnownNat (I.Count (I.FromNat n)),
    KnownNat (I.Count (I.FromNat rows)),
    I.FromLists e (I.FromNat n) (I.FromNat rows),
    I.FromLists e (I.FromNat rows) (I.FromNat rows),
    I.FromNat (rows + n) ~ Either (I.FromNat rows) (I.FromNat n)
  ) =>
  Matrix e rows (rows + n)
i1 = tr p1

i2 ::
  ( Num e,
    KnownNat (I.Count (I.FromNat rows)),
    KnownNat (I.Count (I.FromNat m)),
    I.FromLists e (I.FromNat m) (I.FromNat rows),
    I.FromLists e (I.FromNat rows) (I.FromNat rows),
    I.FromNat (m + rows) ~ Either (I.FromNat m) (I.FromNat rows)
  ) =>
  Matrix e rows (m + rows)
i2 = tr p2

-- Dimensions

rows :: (KnownNat (I.Count (I.FromNat rows))) => Matrix e cols rows -> Int
rows (M m) = I.rows m

columns :: (KnownNat (I.Count (I.FromNat cols))) => Matrix e cols rows -> Int
columns (M m) = I.columns m

-- Coproduct Bifunctor

infixl 5 -|-

(-|-) ::
  ( Num e,
    KnownNat (I.Count (I.FromNat j)),
    KnownNat (I.Count (I.FromNat k)),
    I.FromLists e (I.FromNat k) (I.FromNat k),
    I.FromLists e (I.FromNat j) (I.FromNat k),
    I.FromLists e (I.FromNat k) (I.FromNat j),
    I.FromLists e (I.FromNat j) (I.FromNat j),
    I.FromNat (n + m) ~ Either (I.FromNat n) (I.FromNat m),
    I.FromNat (k + j) ~ Either (I.FromNat k) (I.FromNat j)
  ) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (n + m) (k + j)
(-|-) (M a) (M b) = M ((I.-|-) a b)

-- Khatri Rao Product and projections

kp1 :: 
  forall e m k .
  ( Num e,
    KnownNat (I.Count (I.FromNat m)),
    KnownNat (I.Count (I.FromNat k)),
    KnownNat (I.Count (I.FromNat (m * k))),
    I.FromLists e (I.FromNat (m * k)) (I.FromNat m),
    I.FromNat (m * k) ~ I.FromNat (I.Count (I.FromNat m) * I.Count (I.FromNat k))
  ) => Matrix e (m * k) m
kp1 = M (I.kp1 @e @(I.FromNat m) @(I.FromNat k))

kp2 :: 
    forall e m k.
    ( Num e,
      KnownNat (I.Count (I.FromNat k)),
      I.FromLists e (I.FromNat (m * k)) (I.FromNat k),
      KnownNat (I.Count (I.FromNat m)),
      KnownNat (I.Count (I.FromNat (m * k))),
      I.FromNat (m * k) ~ I.FromNat (I.Count (I.FromNat m) * I.Count (I.FromNat k))
    ) => Matrix e (m * k) k
kp2 = M (I.kp2 @e @(I.FromNat m) @(I.FromNat k))

khatri ::
  forall e cols a b.
  ( Num e,
    KnownNat (I.Count (I.FromNat a)),
    KnownNat (I.Count (I.FromNat b)),
    KnownNat (I.Count (I.FromNat (a * b))),
    I.FromLists e (I.FromNat (a * b)) (I.FromNat a),
    I.FromLists e (I.FromNat (a * b)) (I.FromNat b),
    I.FromNat (a * b) ~ I.FromNat (I.Count (I.FromNat a) * I.Count (I.FromNat b))
  ) => Matrix e cols a -> Matrix e cols b -> Matrix e cols (a * b)
khatri a b =
  let kp1' = kp1 @e @a @b
      kp2' = kp2 @e @a @b
   in comp (tr kp1') a * comp (tr kp2') b

-- Product Bifunctor (Kronecker)

infixl 4 ><

(><) ::
  forall e m p n q.
  ( Num e,
    KnownNat (I.Count (I.FromNat m)),
    KnownNat (I.Count (I.FromNat n)),
    KnownNat (I.Count (I.FromNat p)),
    KnownNat (I.Count (I.FromNat q)),
    KnownNat (I.Count (I.FromNat (m * n))),
    KnownNat (I.Count (I.FromNat (p * q))),
    I.FromLists e (I.FromNat (m * n)) (I.FromNat m),
    I.FromLists e (I.FromNat (m * n)) (I.FromNat n),
    I.FromLists e (I.FromNat (p * q)) (I.FromNat p),
    I.FromLists e (I.FromNat (p * q)) (I.FromNat q),
    I.FromNat (m * n) ~ I.FromNat (I.Count (I.FromNat m) * I.Count (I.FromNat n)),
    I.FromNat (p * q) ~ I.FromNat (I.Count (I.FromNat p) * I.Count (I.FromNat q))
  ) => Matrix e m p -> Matrix e n q -> Matrix e (m * n) (p * q)
(><) a b =
  let kp1' = kp1 @e @m @n
      kp2' = kp2 @e @m @n
   in khatri (comp a kp1') (comp b kp2')

-- Matrix abide Junc Split

abideJS :: Matrix e cols rows -> Matrix e cols rows
abideJS (M m) = M (I.abideJS m)

-- Matrix abide Split Junc

abideSJ :: Matrix e cols rows -> Matrix e cols rows
abideSJ (M m) = M (I.abideSJ m)

-- Matrix transposition

tr :: Matrix e cols rows -> Matrix e rows cols
tr (M m) = M (I.tr m)

-- Selective 'select' operator
select :: 
       ( Num e,
         I.FromLists e (I.FromNat rows1) (I.FromNat rows1),
         KnownNat (I.Count (I.FromNat rows1)),
         I.FromNat rows2 ~ I.FromNat rows1,
         I.FromNat cols1 ~ I.FromNat cols2,
         I.FromNat rows3 ~ Either (I.FromNat cols3) (I.FromNat rows1)
       ) => Matrix e cols1 rows3 -> Matrix e cols3 rows1 -> Matrix e cols2 rows2
select (M m) (M y) = M (I.select m y)

-- McCarthy's Conditional

cond ::
     ( I.FromNat (I.Count (I.FromNat cols)) ~ I.FromNat cols,
       KnownNat (I.Count (I.FromNat cols)),
       I.FromLists e () (I.FromNat cols),
       I.FromLists e (I.FromNat cols) (),
       I.FromLists e (I.FromNat cols) (I.FromNat cols),
       Bounded a,
       Enum a,
       Num e,
       Ord e
     )
     =>
     (a -> Bool) -> Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
cond p (M a) (M b) = M (I.cond p a b)

-- Pretty print

pretty :: (KnownNat (I.Count (I.FromNat cols)), Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

prettyPrint :: (KnownNat (I.Count (I.FromNat cols)), Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m
