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
-- Module     : LAoP.Matrix.Nat
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

module LAoP.Matrix.Nat
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

    -- * Constraint type synonyms
    Countable,
    CountableDimensions,
    CountableN,
    CountableNz,
    CountableDimensionsN,
    FromListsN,
    FromListsNz,
    Liftable,
    TrivialE,
    TrivialP,

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

    -- * Matrix printing
    pretty,
    prettyPrint
  )
where

import Data.Proxy
import GHC.TypeLits
import Control.DeepSeq
import qualified Control.Category as C
import qualified LAoP.Matrix.Internal as I

newtype Matrix e (cols :: Nat) (rows :: Nat) = M (I.Matrix e (I.FromNat cols) (I.FromNat rows))
  deriving (Show, Num, Eq, Ord, NFData) via (I.Matrix e (I.FromNat cols) (I.FromNat rows))

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a              = KnownNat (I.Count a)
type CountableDimensions a b  = (Countable a, Countable b)
type CountableN a             = KnownNat (I.Count (I.FromNat a))
type CountableNz a            = KnownNat (I.Count (I.Normalize a))
type CountableDimensionsN a b = (CountableN a, CountableN b)
type FromListsN e a b         = I.FromLists e (I.FromNat a) (I.FromNat b)
type FromListsNz e a b        = I.FromLists e (I.Normalize a) (I.Normalize b)
type Liftable e a b           = (Bounded a, Bounded b, Enum a, Enum b, Eq b, Num e, Ord e)
type TrivialE a b             = I.FromNat (a + b) ~ Either (I.FromNat a) (I.FromNat b)
type TrivialP a b             = I.FromNat (a * b) ~ I.FromNat (I.Count (I.FromNat a) * I.Count (I.FromNat b))

-- | It isn't possible to implement the 'id' function so it's
-- implementation is 'undefined'. However 'comp' can be and this partial
-- class implementation exists just to make the code more readable.
--
-- Please use 'iden' instead.
instance (Num e) => C.Category (Matrix e) where
    id = undefined
    (.) = comp

-- Primitives

empty :: Matrix e 0 0
empty = M I.Empty

one :: e -> Matrix e 1 1
one = M . I.One

join ::
  (TrivialE a b) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (a + b) rows
join (M a) (M b) = M (I.Join a b)

infixl 3 |||
(|||) ::
  (TrivialE a b) =>
  Matrix e a rows ->
  Matrix e b rows ->
  Matrix e (a + b) rows
(|||) = join

fork ::
  (TrivialE a b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a + b)
fork (M a) (M b) = M (I.Fork a b)

infixl 2 ===
(===) ::
  (TrivialE a b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a + b)
(===) = fork

-- Construction

fromLists :: (FromListsN e cols rows) => [[e]] -> Matrix e cols rows
fromLists = M . I.fromLists

matrixBuilder ::
  (FromListsN e cols rows, CountableN cols, CountableN rows) 
  => ((Int, Int) -> e) -> Matrix e cols rows
matrixBuilder = M . I.matrixBuilder

col :: (I.FromLists e () (I.FromNat rows)) => [e] -> Matrix e 1 rows
col = M . I.col

row :: (I.FromLists e (I.FromNat cols) ()) => [e] -> Matrix e cols 1
row = M . I.row

fromF' ::
  ( Liftable e a b,
    CountableN cols,
    CountableN rows,
    FromListsN e rows cols
  ) =>
  (a -> b) ->
  Matrix e cols rows
fromF' = M . I.fromF'

fromF ::
  ( Liftable e a b,
    CountableNz a,
    CountableNz b,
    FromListsNz e b a
  ) =>
  (a -> b) ->
  Matrix e (I.Count a) (I.Count b)
fromF = undefined -- M . I.fromF

-- Conversion

toLists :: Matrix e cols rows -> [[e]]
toLists (M m) = I.toLists m

toList :: Matrix e cols rows -> [e]
toList (M m) = I.toList m

-- Zeros Matrix

zeros ::
  (Num e, FromListsN e cols rows, CountableN cols, CountableN rows) =>
  Matrix e cols rows
zeros = M I.zeros

-- Ones Matrix

ones ::
  (Num e, FromListsN e cols rows, CountableN cols, CountableN rows) =>
  Matrix e cols rows
ones = M I.ones

-- Const Matrix

constant ::
  (Num e, FromListsN e cols rows, CountableN cols, CountableN rows) =>
  e ->
  Matrix e cols rows
constant = M . I.constant

-- Bang Matrix

bang ::
  forall e cols.
  (Num e, Enum e, I.FromLists e (I.FromNat cols) (), CountableN cols) =>
  Matrix e cols 1
bang = M I.bang

-- iden Matrix

iden ::
  (Num e, FromListsN e cols cols, CountableN cols) =>
  Matrix e cols cols
iden = M I.iden

-- Matrix composition (MMM)

comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (M a) (M b) = M (I.comp a b)

p1 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e n m,
    FromListsN e m m,
    TrivialE m n
  ) =>
  Matrix e (m + n) m
p1 = M I.p1

p2 ::
  ( Num e,
    CountableDimensionsN n m,
    FromListsN e m n,
    FromListsN e n n,
    TrivialE m n
  ) =>
  Matrix e (m + n) n
p2 = M I.p2

-- Injections

i1 ::
  ( Num e,
    CountableDimensionsN n rows,
    FromListsN e n rows,
    FromListsN e rows rows,
    TrivialE rows n
  ) =>
  Matrix e rows (rows + n)
i1 = tr p1

i2 ::
  ( Num e,
    CountableDimensionsN rows m,
    FromListsN e m rows,
    FromListsN e rows rows,
    TrivialE m rows
  ) =>
  Matrix e rows (m + rows)
i2 = tr p2

-- Dimensions

rows :: (CountableN rows) => Matrix e cols rows -> Int
rows (M m) = I.rows m

columns :: (CountableN cols) => Matrix e cols rows -> Int
columns (M m) = I.columns m

infixl 5 -|-

-- | Coproduct Bifunctor (Direct sum)
(-|-) ::
  ( Num e,
    CountableDimensionsN j k,
    FromListsN e k k,
    FromListsN e j k,
    FromListsN e k j,
    FromListsN e j j,
    TrivialE n m,
    TrivialE k j
  ) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (n + m) (k + j)
(-|-) (M a) (M b) = M ((I.-|-) a b)

-- | Khatri Rao Product first projection
fstM ::
  forall e m k .
  ( Num e,
    CountableDimensionsN m k,
    CountableN (m * k),
    FromListsN e (m * k) m,
    TrivialP m k
  ) => Matrix e (m * k) m
fstM = M (I.fstM @e @(I.FromNat m) @(I.FromNat k))

-- | Khatri Rao Product second projection
sndM ::
    forall e m k.
    ( Num e,
      CountableDimensionsN k m,
      FromListsN e (m * k) k,
      CountableN (m * k),
      TrivialP m k
    ) => Matrix e (m * k) k
sndM = M (I.sndM @e @(I.FromNat m) @(I.FromNat k))

-- | Khatri Rao Product
kr ::
  forall e cols a b.
  ( Num e,
    CountableDimensionsN a b,
    CountableN (a * b),
    FromListsN e (a * b) a,
    FromListsN e (a * b) b,
    TrivialP a b
  ) => Matrix e cols a -> Matrix e cols b -> Matrix e cols (a * b)
kr a b =
  let fstM' = fstM @e @a @b
      sndM' = sndM @e @a @b
   in comp (tr fstM') a * comp (tr sndM') b

-- | Product Bifunctor (Kronecker)
infixl 4 ><

(><) ::
  forall e m p n q.
  ( Num e,
    CountableDimensionsN m n,
    CountableDimensionsN p q,
    CountableDimensionsN (m * n) (p * q),
    FromListsN e (m * n) m,
    FromListsN e (m * n) n,
    FromListsN e (p * q) p,
    FromListsN e (p * q) q,
    TrivialP m n,
    TrivialP p q
  ) => Matrix e m p -> Matrix e n q -> Matrix e (m * n) (p * q)
(><) a b =
  let fstM' = fstM @e @m @n
      sndM' = sndM @e @m @n
   in kr (comp a fstM') (comp b sndM')

-- | Matrix abide Join Fork
abideJF :: Matrix e cols rows -> Matrix e cols rows
abideJF (M m) = M (I.abideJF m)

-- | Matrix abide Fork Join
abideFJ :: Matrix e cols rows -> Matrix e cols rows
abideFJ (M m) = M (I.abideFJ m)

-- | Matrix transposition
tr :: Matrix e cols rows -> Matrix e rows cols
tr (M m) = M (I.tr m)

-- Selective 'select' operator
select :: 
       ( Num e,
         FromListsN e rows1 rows1,
         CountableN rows1,
         I.FromNat rows2 ~ I.FromNat rows1,
         I.FromNat cols1 ~ I.FromNat cols2,
         I.FromNat rows3 ~ Either (I.FromNat cols3) (I.FromNat rows1)
       ) => Matrix e cols1 rows3 -> Matrix e cols3 rows1 -> Matrix e cols2 rows2
select (M m) (M y) = M (I.select m y)

-- McCarthy's Conditional

cond ::
     ( I.FromNat (I.Count (I.FromNat cols)) ~ I.FromNat cols,
       CountableN cols,
       I.FromLists e () (I.FromNat cols),
       I.FromLists e (I.FromNat cols) (),
       FromListsN e cols cols,
       Liftable e a Bool
     )
     =>
     (a -> Bool) -> Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
cond p (M a) (M b) = M (I.cond p a b)

-- Pretty print

pretty :: (CountableDimensionsN cols rows, Show e) => Matrix e cols rows -> String
pretty (M m) = I.pretty m

prettyPrint :: (CountableDimensionsN cols rows, Show e) => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m

-- | Zip two matrices with a given binary function
zipWithM :: (e -> f -> g) -> Matrix e cols rows -> Matrix f cols rows -> Matrix g cols rows
zipWithM f (M a) (M b) = M (I.zipWithM f a b)
