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

    -- ** Matrix "abiding"
    abideJS,
    abideSJ,

    -- * Biproduct approach
    -- ** Split
    split,
    -- *** Projections
    p1,
    p2,
    -- ** Junc
    junc,
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
    KhatriP1,
    kp1,
    KhatriP2,
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

    -- * Matrix printing
    pretty,
    prettyPrint
  )
where

import Data.Proxy
import GHC.TypeLits
import qualified Matrix.Type as I

newtype Matrix e (cols :: Nat) (rows :: Nat) = M (I.Matrix e (I.FromNat cols) (I.FromNat rows))
  deriving (Show, Num, Eq) via (I.Matrix e (I.FromNat cols) (I.FromNat rows))

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

split ::
  (I.FromNat rows3 ~ Either (I.FromNat rows1) (I.FromNat rows2)) =>
  Matrix e cols rows1 ->
  Matrix e cols rows2 ->
  Matrix e cols rows3
split (M a) (M b) = M (I.Split a b)

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

class KhatriP1 e (m :: Nat) (k :: Nat) where
  kp1 :: Matrix e (m * k) m

instance KhatriP1 e 0 k where
  kp1 = empty

instance
  forall e m k.
  ( Num e,
    KnownNat k,
    I.FromLists e (I.FromNat (m * k)) (I.FromNat m),
    KnownNat (I.Count (I.FromNat m)),
    KnownNat (I.Count (I.FromNat (m * k)))
  ) =>
  KhatriP1 e m k
  where
  kp1 = matrixBuilder f
    where
      offset = fromInteger (natVal (Proxy :: Proxy k))
      f (x, y)
        | y >= (x * offset) && y <= (x * offset + offset - 1) = 1
        | otherwise = 0

class KhatriP2 e (m :: Nat) (k :: Nat) where
  kp2 :: Matrix e (m * k) k

instance KhatriP2 e m 0 where
  kp2 = empty

instance
  forall e m k.
  ( Num e,
    KnownNat k,
    KnownNat (I.Count (I.FromNat k)),
    I.FromLists e (I.FromNat (m * k)) (I.FromNat k),
    KnownNat (I.Count (I.FromNat m)),
    KnownNat (I.Count (I.FromNat (m * k)))
  ) =>
  KhatriP2 e m k
  where
  kp2 = matrixBuilder f
    where
      offset = fromInteger (natVal (Proxy :: Proxy k))
      f (x, y)
        | x == y || mod (y - x) offset == 0 = 1
        | otherwise = 0

khatri ::
  forall e cols a b.
  (Num e, KhatriP1 e a b, KhatriP2 e a b) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (a * b)
khatri a b =
  let kp1' = kp1 @e @a @b
      kp2' = kp2 @e @a @b
   in comp (tr kp1') a * comp (tr kp2') b

-- Product Bifunctor (Kronecker)

infixl 4 ><

(><) ::
  forall e m p n q.
  (Num e, KhatriP1 e m n, KhatriP2 e m n, KhatriP1 e p q, KhatriP2 e p q) =>
  Matrix e m p ->
  Matrix e n q ->
  Matrix e (m * n) (p * q)
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

-- Pretty print

pretty :: Show e => Matrix e cols rows -> String
pretty (M m) = I.pretty m

prettyPrint :: Show e => Matrix e cols rows -> IO ()
prettyPrint (M m) = I.prettyPrint m
