{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Matrix.Type
  ( -- | LAoP (Linear Algebra of Programming) Inductive Matrix definition.
    --
    --         LAoP generalises relations and functions treating them as
    --         Boolean matrices and in turn consider these as arrows.
    --         This library offers many of the combinators mentioned in the work of
    --         Macedo (2012) and Oliveira (2012).
    --
    --          This definition makes use of the fact that 'Void' is
    --          isomorphic to 0 and '()' to 1 and captures matrix
    --          dimensions as stacks of 'Either's.
    --
    --          There exists two type families that make it easier to write
    --          matrix dimensions: 'FromNat' and 'Count'. This approach
    --          leads to a very straightforward implementation 
    --          of LAoP combinators. 

    -- * Type safe matrix representation
    Matrix (..),

    -- * Primitives
    empty,
    one,
    junc,
    split,

    -- * Auxiliary type families
    FromNat,
    Count,
    Normalize,

    -- * Matrix construction and conversion
    FromLists,
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
    fromF',

    -- * Matrix printing
    pretty,
    prettyPrint
  )
    where

import Utils
import Data.Bool
import Data.Kind
import Data.List
import Data.Proxy
import Data.Void
import GHC.TypeLits

-- | Matrix data type
data Matrix e cols rows where
  Empty :: Matrix e Void Void
  One :: e -> Matrix e () ()
  Junc :: Matrix e a rows -> Matrix e b rows -> Matrix e (Either a b) rows
  Split :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (Either a b)

deriving instance (Show e) => Show (Matrix e cols rows)

-- | Type family that computes the cardinality of a given type dimension
type family Count (d :: Type) where
  Count Void = 0
  Count () = 1
  Count (Either a b) = (+) (Count a) (Count b)
  Count (a, b) = (*) (Count a) (Count b)
  Count (a -> b) = (^) (Count b) (Count a)
  Count (Natural n) = n

-- | Type family that computes of a given type dimension from a fiven natural
type family FromNat (n :: Nat) where
  FromNat 0 = Void
  FromNat 1 = ()
  FromNat n = Either () (FromNat (n - 1)) 

-- | Type family that normalizes the representation of a given data
-- structure
type family Normalize (d :: Type) where
  Normalize d = FromNat (Count d)

instance Eq e => Eq (Matrix e cols rows) where
  Empty == Empty             = True
  (One a) == (One b)         = a == b
  (Junc a b) == (Junc c d)   = a == c && b == d
  (Split a b) == (Split c d) = a == c && b == d

instance Num e => Num (Matrix e cols rows) where

  Empty + Empty             = Empty
  (One a) + (One b)         = One (a + b)
  (Junc a b) + (Junc c d)   = Junc (a + c) (b + d)
  (Split a b) + (Split c d) = Split (a + c) (b + d)

  Empty - Empty             = Empty
  (One a) - (One b)         = One (a - b)
  (Junc a b) - (Junc c d)   = Junc (a - c) (b - d)
  (Split a b) - (Split c d) = Split (a - c) (b - d)

  Empty * Empty             = Empty
  (One a) * (One b)         = One (a * b)
  (Junc a b) * (Junc c d)   = Junc (a * c) (b * d)
  (Split a b) * (Split c d) = Split (a * c) (b * d)

  abs Empty       = Empty
  abs (One a)     = One (abs a)
  abs (Junc a b)  = Junc (abs a) (abs b)
  abs (Split a b) = Split (abs a) (abs b)

  signum Empty       = Empty
  signum (One a)     = One (signum a)
  signum (Junc a b)  = Junc (signum a) (signum b)
  signum (Split a b) = Split (signum a) (signum b)

-- Primitives

empty :: Matrix e Void Void
empty = Empty

one :: e -> Matrix e () ()
one = One

junc :: Matrix e a rows -> Matrix e b rows -> Matrix e (Either a b) rows
junc = Junc

infixl 3 |||
(|||) :: Matrix e a rows -> Matrix e b rows -> Matrix e (Either a b) rows
(|||) = Junc

split :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (Either a b)
split = Split

infixl 2 ===
(===) :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (Either a b)
(===) = Split

-- Construction

class FromLists e cols rows where
  fromLists :: [[e]] -> Matrix e cols rows

instance FromLists e Void Void where
  fromLists [] = Empty
  fromLists _  = error "Wrong dimensions"

instance {-# OVERLAPPING #-} FromLists e () () where
  fromLists [[e]] = One e
  fromLists _     = error "Wrong dimensions"

instance (FromLists e cols ()) => FromLists e (Either () cols) () where
  fromLists [h : t] = Junc (One h) (fromLists [t])
  fromLists _       = error "Wrong dimensions"

instance (FromLists e () rows) => FromLists e () (Either () rows) where
  fromLists ([h] : t) = Split (One h) (fromLists t)
  fromLists _         = error "Wrong dimensions"

instance {-# OVERLAPPABLE #-} (FromLists e cols c, FromLists e cols d) => FromLists e cols (Either c d) where
  fromLists (h : t) =
    let lh        = length h
        condition = all (== lh) (map length t)
     in if lh > 0 && condition
          then Split (fromLists [h]) (fromLists t)
          else error "Not all rows have the same length"

matrixBuilder ::
  forall e cols rows.
  ( FromLists e cols rows,
    KnownNat (Count cols),
    KnownNat (Count rows)
  ) =>
  ((Int, Int) -> e) ->
  Matrix e cols rows
matrixBuilder f =
  let c         = fromInteger $ natVal (Proxy :: Proxy (Count cols))
      r         = fromInteger $ natVal (Proxy :: Proxy (Count rows))
      positions = [(a, b) | a <- [0 .. (r - 1)], b <- [0 .. (c - 1)]]
   in fromLists . map (map f) . groupBy (\(x, _) (w, _) -> x == w) $ positions

col :: (FromLists e () rows) => [e] -> Matrix e () rows
col = fromLists . map (: [])

row :: (FromLists e cols ()) => [e] -> Matrix e cols ()
row = fromLists . (: [])

fromF ::
  forall a b cols rows e.
  ( Bounded a,
    Bounded b,
    Enum a,
    Enum b,
    Eq b,
    Num e,
    Ord e,
    KnownNat (Count cols),
    KnownNat (Count rows),
    FromLists e rows cols
  ) =>
  (a -> b) ->
  Matrix e cols rows
fromF f =
  let minA         = minBound @a
      maxA         = maxBound @a
      minB         = minBound @b
      maxB         = maxBound @b
      ccols        = fromInteger $ natVal (Proxy :: Proxy (Count cols))
      rrows        = fromInteger $ natVal (Proxy :: Proxy (Count rows))
      elementsA    = take ccols [minA .. maxA]
      elementsB    = take rrows [minB .. maxB]
      combinations = (,) <$> elementsA <*> elementsB
      combAp       = map snd . sort . map (\(a, b) -> if f a == b 
                                                         then ((fromEnum a, fromEnum b), 1) 
                                                         else ((fromEnum a, fromEnum b), 0)) $ combinations
      mList        = buildList combAp rrows
   in tr $ fromLists mList
  where
    buildList [] _ = []
    buildList l r  = take r l : buildList (drop r l) r

fromF' ::
  forall a b e.
  ( Bounded a,
    Bounded b,
    Enum a,
    Enum b,
    Eq b,
    Num e,
    Ord e,
    KnownNat (Count (Normalize a)),
    KnownNat (Count (Normalize b)),
    FromLists e (Normalize b) (Normalize a)
  ) =>
  (a -> b) ->
  Matrix e (Normalize a) (Normalize b)
fromF' f =
  let minA         = minBound @a
      maxA         = maxBound @a
      minB         = minBound @b
      maxB         = maxBound @b
      ccols        = fromInteger $ natVal (Proxy :: Proxy (Count (Normalize a)))
      rrows        = fromInteger $ natVal (Proxy :: Proxy (Count (Normalize b)))
      elementsA    = take ccols [minA .. maxA]
      elementsB    = take rrows [minB .. maxB]
      combinations = (,) <$> elementsA <*> elementsB
      combAp       = map snd . sort . map (\(a, b) -> if f a == b 
                                                         then ((fromEnum a, fromEnum b), 1) 
                                                         else ((fromEnum a, fromEnum b), 0)) $ combinations
      mList        = buildList combAp rrows
   in tr $ fromLists mList
  where
    buildList [] _ = []
    buildList l r  = take r l : buildList (drop r l) r

-- Conversion

toLists :: Matrix e cols rows -> [[e]]
toLists Empty       = []
toLists (One e)     = [[e]]
toLists (Split l r) = toLists l ++ toLists r
toLists (Junc l r)  = zipWith (++) (toLists l) (toLists r)

toList :: Matrix e cols rows -> [e]
toList = concat . toLists

-- Zeros Matrix

zeros :: (Num e, FromLists e cols rows, KnownNat (Count cols), KnownNat (Count rows)) => Matrix e cols rows
zeros = matrixBuilder (const 0)

-- Ones Matrix

ones :: (Num e, FromLists e cols rows, KnownNat (Count cols), KnownNat (Count rows)) => Matrix e cols rows
ones = matrixBuilder (const 1)

-- Const Matrix

constant :: (Num e, FromLists e cols rows, KnownNat (Count cols), KnownNat (Count rows)) => e -> Matrix e cols rows
constant e = matrixBuilder (const e)

-- Bang Matrix

bang :: forall e cols. (Num e, Enum e, FromLists e cols (), KnownNat (Count cols)) => Matrix e cols ()
bang =
  let c = fromInteger $ natVal (Proxy :: Proxy (Count cols))
   in fromLists [take c [1, 1 ..]]

-- Identity Matrix

identity :: (Num e, FromLists e cols cols, KnownNat (Count cols)) => Matrix e cols cols
identity = matrixBuilder (bool 0 1 . uncurry (==))

-- Matrix composition (MMM)

comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp Empty Empty            = Empty
comp (One a) (One b)        = One (a * b)
comp (Junc a b) (Split c d) = comp a c + comp b d         -- Divide-and-conquer law
comp (Split a b) c          = Split (comp a c) (comp b c) -- Split fusion law
comp c (Junc a b)           = Junc (comp c a) (comp c b)  -- Junc fusion law

-- Projections

p1 :: forall e m n. (Num e, KnownNat (Count n), KnownNat (Count m), FromLists e n m, FromLists e m m) => Matrix e (Either m n) m
p1 =
  let iden = identity :: Matrix e m m
      zero = zeros :: Matrix e n m
   in junc iden zero

p2 :: forall e m n. (Num e, KnownNat (Count n), KnownNat (Count m), FromLists e m n, FromLists e n n) => Matrix e (Either m n) n
p2 =
  let iden = identity :: Matrix e n n
      zero = zeros :: Matrix e m n
   in junc zero iden

-- Injections

i1 :: (Num e, KnownNat (Count n), KnownNat (Count m), FromLists e n m, FromLists e m m) => Matrix e m (Either m n)
i1 = tr p1

i2 :: (Num e, KnownNat (Count n), KnownNat (Count m), FromLists e m n, FromLists e n n) => Matrix e n (Either m n)
i2 = tr p2

-- Dimensions

rows :: forall e cols rows. (KnownNat (Count rows)) => Matrix e cols rows -> Int
rows _ = fromInteger $ natVal (Proxy :: Proxy (Count rows))

columns :: forall e cols rows. (KnownNat (Count cols)) => Matrix e cols rows -> Int
columns _ = fromInteger $ natVal (Proxy :: Proxy (Count cols))

-- Coproduct Bifunctor

infixl 5 -|-

(-|-) ::
  forall e n k m j.
  ( Num e,
    KnownNat (Count j),
    KnownNat (Count k),
    FromLists e k k,
    FromLists e j k,
    FromLists e k j,
    FromLists e j j
  ) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (Either n m) (Either k j)
(-|-) a b = Junc (comp i1 a) (comp i2 b)

-- Khatri Rao Product and projections

class KhatriP1 e m k where
  kp1 :: Matrix e (Normalize (m, k)) m

instance KhatriP1 e Void k where
  kp1 = Empty

instance
  ( Num e,
    KnownNat (Count k),
    FromLists e (FromNat (Count m * Count k)) m,
    KnownNat (Count m),
    KnownNat (Count (Normalize (m, k)))
  ) =>
  KhatriP1 e m k
  where
  kp1 = matrixBuilder f
    where
      offset = fromInteger (natVal (Proxy :: Proxy (Count k)))
      f (x, y)
        | y >= (x * offset) && y <= (x * offset + offset - 1) = 1
        | otherwise = 0

class KhatriP2 e m k where
  kp2 :: Matrix e (Normalize (m, k)) k

instance KhatriP2 e m Void where
  kp2 = Empty

instance
  ( Num e,
    KnownNat (Count k),
    FromLists e (FromNat (Count m * Count k)) k,
    KnownNat (Count m),
    KnownNat (Count (Normalize (m, k)))
  ) =>
  KhatriP2 e m k
  where
  kp2 = matrixBuilder f
    where
      offset = fromInteger (natVal (Proxy :: Proxy (Count k)))
      f (x, y)
        | x == y || mod (y - x) offset == 0 = 1
        | otherwise                         = 0

khatri :: forall e cols a b. (Num e, KhatriP1 e a b, KhatriP2 e a b) => Matrix e cols a -> Matrix e cols b -> Matrix e cols (Normalize (a, b))
khatri a b =
  let kp1' = kp1 @e @a @b
      kp2' = kp2 @e @a @b
   in comp (tr kp1') a * comp (tr kp2') b

-- Product Bifunctor (Kronecker)

infixl 4 ><

(><) :: forall e m p n q. (Num e, KhatriP1 e m n, KhatriP2 e m n, KhatriP1 e p q, KhatriP2 e p q) => Matrix e m p -> Matrix e n q -> Matrix e (Normalize (m, n)) (Normalize (p, q))
(><) a b =
  let kp1' = kp1 @e @m @n
      kp2' = kp2 @e @m @n
   in khatri (comp a kp1') (comp b kp2')

-- Matrix abide Junc Split

abideJS :: Matrix e cols rows -> Matrix e cols rows
abideJS (Junc (Split a c) (Split b d)) = Split (Junc (abideJS a) (abideJS b)) (Junc (abideJS c) (abideJS d)) -- Junc-Split abide law
abideJS Empty                          = Empty
abideJS (One e)                        = One e
abideJS (Junc a b)                     = Junc (abideJS a) (abideJS b)
abideJS (Split a b)                    = Split (abideJS a) (abideJS b)

-- Matrix abide Split Junc

abideSJ :: Matrix e cols rows -> Matrix e cols rows
abideSJ (Split (Junc a b) (Junc c d)) = Junc (Split (abideSJ a) (abideSJ c)) (Split (abideSJ b) (abideSJ d)) -- Split-Junc abide law
abideSJ Empty                         = Empty
abideSJ (One e)                       = One e
abideSJ (Junc a b)                    = Junc (abideSJ a) (abideSJ b)
abideSJ (Split a b)                   = Split (abideSJ a) (abideSJ b)

-- Matrix transposition

tr :: Matrix e cols rows -> Matrix e rows cols
tr Empty       = Empty
tr (One e)     = One e
tr (Junc a b)  = Split (tr a) (tr b)
tr (Split a b) = Junc (tr a) (tr b)

-- Selective 'select' operator

select :: (Bounded a1, Bounded b, Enum a1, Enum b, Num e, Ord e,
                 KnownNat (Count a2), KnownNat (Count rows), FromLists e rows a2,
                 FromLists e rows rows, Eq b) =>
                Matrix e cols (Either a2 rows) -> (a1 -> b) -> Matrix e cols rows
select m y = 
    let f = fromF y
     in junc f identity `comp` m

-- McCarthy's Conditional

cond ::
     ( cols ~ FromNat (Count cols),
       KnownNat (Count cols),
       FromLists e () cols,
       FromLists e cols (),
       FromLists e cols cols,
       KhatriP2 e () cols,
       Bounded a,
       Enum a,
       Num e,
       Ord e
     )
     =>
     (a -> Bool) -> Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
cond p f g = junc f g `comp` grd p

grd :: 
    ( q ~ FromNat (Count q),
      KnownNat (Count q),
      FromLists e () q,
      FromLists e q (),
      FromLists e q q,
      KhatriP2 e () q,
      Bounded a,
      Enum a,
      Num e,
      Ord e
    )
    =>
    (a -> Bool) -> Matrix e q (Either q q)
grd f = split (corr f) (corr (not . f))

corr :: 
    forall e a q . 
    ( q ~ FromNat (Count q),
      KnownNat (Count q),
      FromLists e () q,
      FromLists e q (),
      FromLists e q q,
      KhatriP2 e () q,
      Bounded a,
      Enum a,
      Num e,
      Ord e
    ) 
     => (a -> Bool) -> Matrix e q q
corr p = let f = fromF p :: Matrix e q ()
          in khatri f (identity :: Matrix e q q)

-- Pretty print

prettyAux :: Show e => [[e]] -> [[e]] -> String
prettyAux [] _     = ""
prettyAux [[e]] m   = "│ " ++ fill (show e) ++ " │\n"
  where
   v  = fmap show m
   widest = maximum $ fmap length v
   fill str = replicate (widest - length str - 2) ' ' ++ str
prettyAux [h] m     = "│ " ++ fill (head $ map show h) ++ " │\n"
  where
   v  = fmap show m
   widest = maximum $ fmap length v
   fill str = replicate (widest - length str - 2) ' ' ++ str
prettyAux (h : t) l = "│ " ++ fill (head $ map show h) ++ " │\n" ++ 
                      prettyAux t l
  where
   v  = fmap show l
   widest = maximum $ fmap length v
   fill str = replicate (widest - length str - 2) ' ' ++ str

pretty :: (KnownNat (Count cols), Show e) => Matrix e cols rows -> String
pretty m = "┌ " ++ blank ++ " ┐\n" ++ 
            prettyAux (toLists m) (toLists m) ++
            "└ " ++ blank ++ " ┘"
  where
   v  = fmap show (toList m)
   widest = maximum $ fmap length v
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""

prettyPrint :: (KnownNat (Count cols), Show e) => Matrix e cols rows -> IO ()
prettyPrint = putStrLn . pretty
