{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LAoP.Utils.Internal
  ( -- | This is an internal module and it is not meant to be imported.
    --
    -- Utility module that provides the 'Natural' data type.
    -- The semantic associated with this data type is that
    -- it's meant to be a restricted 'Int' value. For example
    -- the type @Natural 6@ can only be instanciated with @Nat n@
    -- where @0 <= n <= 6@. Why, You might ask, because with normal
    -- 'Int's it is not possible to have a decent @Enum (Int, Int)@
    -- instance. See the following probabilistic programming model as and
    -- example:
    --
    -- We want to calculate the probability of the sum of two dice throws.
    -- To do this we start by defining the sample space:
    --
    -- @
    -- type SampleSpace = Int -- We think 'Int' are enough
    --
    -- die :: Dist Int 6
    -- die = unifrom [1..6]
    --
    -- -- Promote 'Int' addition to a matrix
    -- addM = fromF (uncurry (+)) -- Impossible
    -- @
    --
    -- The last line is impossible because @(Int, Int)@ does not have
    -- a good 'Enum' instance: @[(0, 1), (0, 2), .. (0, maxBound), (1, 0),
    -- ..]@. And we'd like the addition matrix to be of 36 columns by 12
    -- rows but limited to integers up to @6@!
    --
    -- One way to solve this issue is by defining and auxilary data type to
    -- represent the sample space:
    --
    -- @
    -- data SampleSpace = S1 | S2 | S3 | S4 | S5 | S6
    --   deriving (Show, Eq, Enum, Bounded) -- Enum and Bounded are
    --   important
    -- @
    --
    -- And write the sample space addition function:
    --
    -- @
    -- ssAdd :: SampleSpace -> SampleSpace -> Int
    -- ssAdd a b = (fromEnum a + 1) + (fromEnum b + 1)
    -- @
    --
    -- And then promote that function to matrix and everything is alright:
    --
    -- @
    -- ssAddM = fromF' (uncurry ssAdd)
    --
    -- dieSumProb = ssAddM `comp` (khatri die die)
    -- @
    --
    -- This is a nice solution for small sample spaces. But for larger ones
    -- it is not feasible to write a data type with hundreds of constructors
    -- and then write manipulation functions that need to deal with them.
    -- To mitigate this limitation the 'Natural' type comes a long way and
    -- allows one to model the sample in an easier way. See for instance:
    --
    -- @
    -- ssAdd :: Natural 6 -> Natural 6 -> Natural 12
    -- ssAdd = coerceNat (+)
    --
    -- ssAddM = fromF' (uncurry sumSS)
    --
    -- die :: Dist (Natural 6) 6
    -- die = uniform [nat @6 1 .. nat 6]
    --
    -- dieSumProb = ssAddM `comp` (khatri die die)
    -- @
    --
    -- * 'Natural' data type
    Natural(..),
    nat,

    -- * Coerce auxiliar functions to help promote 'Int' typed functions to
    -- 'Natural' typed functions.
    coerceNat,
    coerceNat2,
    coerceNat3,

    -- * Powerset data type
    Powerset (..)
  )
where

import Data.Coerce
import Data.Proxy
import Data.List
import Data.Maybe
import GHC.TypeLits
import Control.DeepSeq

-- | Wrapper around 'Int's that have a restrictive semantic associated.
-- A value of type @'Natural' n m@ can only be instanciated with some 'Int'
-- @i@ that's @n <= i <= m@.
newtype Natural (start :: Nat) (end :: Nat) = Nat Int
  deriving (Show, Read, Eq, Ord, NFData)

-- | Throws a runtime error if any of the operations overflows or
-- underflows.
instance (KnownNat n, KnownNat m) => Num (Natural n m) where
    (Nat a) + (Nat b) = nat @n @m (a + b)
    (Nat a) - (Nat b) = nat @n @m (a - b)
    (Nat a) * (Nat b) = nat @n @m (a * b)
    abs (Nat a) = nat @n @m (abs a)
    signum (Nat a) = nat @n @m (signum a)
    fromInteger i = nat @n @m (fromInteger i)

-- | Natural constructor function. Throws a runtime error if the 'Int'
-- value is greater than the corresponding @m@ or lower than @n@ in the @'Natural' n m@ type.
nat :: forall n m . (KnownNat n, KnownNat m) => Int -> Natural n m
nat i =
  let start = fromInteger (natVal (Proxy :: Proxy n))
      end   = fromInteger (natVal (Proxy :: Proxy m))
   in if start <= i && i <= end
        then Nat i
        else error "Off limits"

-- | Auxiliary function that promotes binary 'Int' functions to 'Natural'
-- binary functions.
coerceNat :: (Int -> Int -> Int) -> (Natural a a' -> Natural b b' -> Natural c c')
coerceNat = coerce

-- | Auxiliary function that promotes ternary (binary) 'Int' functions to 'Natural'
-- functions.
coerceNat2 :: ((Int, Int) -> Int -> Int) -> ((Natural a a', Natural b b') -> Natural c c' -> Natural d d')
coerceNat2 = coerce

-- | Auxiliary function that promotes ternary (binary) 'Int' functions to 'Natural'
-- functions.
coerceNat3 :: (Int -> Int -> a) -> (Natural b b' -> Natural c c' -> a) 
coerceNat3 = coerce

instance (KnownNat n, KnownNat m) => Bounded (Natural n m) where
  minBound = Nat $ fromInteger (natVal (Proxy :: Proxy n))
  maxBound = Nat $ fromInteger (natVal (Proxy :: Proxy m))

instance (KnownNat n, KnownNat m) => Enum (Natural n m) where
  toEnum i = 
      let start = fromInteger (natVal (Proxy :: Proxy n))
       in nat (start + i)
  -- | Throws a runtime error if the value is off limits
  fromEnum (Nat nat) =
    let start = fromInteger (natVal (Proxy :: Proxy n))
        end   = fromInteger (natVal (Proxy :: Proxy m))
     in if start <= nat && nat <= end
          then nat - start
          else error "Off limits"

-- | Optimized 'Enum' instance for tuples that comply with the given
-- constraints.
instance
  ( Enum a,
    Enum b,
    Bounded b
  ) =>
  Enum (a, b)
  where

  toEnum i =
    let (listB :: [b]) = [minBound .. maxBound]
        lengthB = length listB
        fstI = div i lengthB
        sndI = mod i lengthB
     in (toEnum fstI, toEnum sndI)

  fromEnum (a, b) =
    let (listB :: [b]) = [minBound .. maxBound]
        lengthB = length listB
        fstI = fromEnum a
        sndI = fromEnum b
     in fstI * lengthB + sndI

instance
  ( Bounded a,
    Bounded b
  ) => Bounded (Either a b)
  where
  minBound = Left (minBound :: a)
  maxBound = Right (maxBound :: b)

instance
  ( Enum a,
    Bounded a,
    Enum b,
    Bounded b
  ) => Enum (Either a b)
  where
  toEnum i =
      let la = fmap Left ([minBound..maxBound] :: [a])
          lb = fmap Right ([minBound..maxBound] :: [b])
       in (la ++ lb) !! i

  fromEnum (Left a) = fromEnum a
  fromEnum (Right b) = fromEnum (maxBound :: a) + fromEnum b + 1

-- | Powerset data type.
--
-- This data type is a newtype wrapper around '[]'. This exists in order to
-- implement an 'Enum' and 'Bounded' instance that cannot be harmful for the outside.
newtype Powerset a = PS [a]
  deriving (Eq, Show, Read)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x:ps | ps <- powerset xs]

instance
  ( Enum a,
    Bounded a
  ) => Bounded (Powerset a)
  where
  minBound = PS [] 
  maxBound = PS [minBound .. maxBound]

instance
  ( Bounded a,
    Enum a,
    Eq a
  ) => Enum (Powerset a)
  where
  toEnum i =
    let as = [minBound .. maxBound]
        in PS (powerset as !! i)

  fromEnum (PS []) = 0
  fromEnum (PS x) = 
    let as = [minBound .. maxBound]
        in fromMaybe (error "Does not exist") $ elemIndex x (powerset as)
