{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module LAoP.Utils.Internal
  (
    -- * 'Natural' data type
    Natural(..),
    reifyToNatural,

    -- * Coerce auxiliar functions to help promote 'Int' typed functions to
    -- 'Natural' typed functions.
    coerceNat,
    coerceNat2,
    coerceNat3,

    -- * 'List' data type
    List (..),

    -- * Category type class
    Category(..)
  )
where

import Data.Coerce
import Data.Proxy
import Data.List
import Data.Maybe
import GHC.TypeLits hiding (Natural)
import Control.DeepSeq
import GHC.Generics
import Data.Kind
import Prelude hiding (id, (.))
import qualified Prelude

-- | Wrapper around 'Int's that have a restrictive semantic associated.
-- A value of type @'Natural' n m@ can only be instanciated with some 'Int'
-- @i@ that's @n <= i <= m@.
newtype Natural (start :: Nat) (end :: Nat) = Nat Int
  deriving (Show, Read, Eq, Ord, NFData, Generic)

-- | Throws a runtime error if any of the operations overflows or
-- underflows.
instance (KnownNat n, KnownNat m) => Num (Natural n m) where
    (Nat a) + (Nat b) = reifyToNatural @n @m (a + b)
    (Nat a) - (Nat b) = reifyToNatural @n @m (a - b)
    (Nat a) * (Nat b) = reifyToNatural @n @m (a * b)
    abs (Nat a) = reifyToNatural @n @m (abs a)
    signum (Nat a) = reifyToNatural @n @m (signum a)
    fromInteger i = reifyToNatural @n @m (fromInteger i)

-- | Natural constructor function. Throws a runtime error if the 'Int' value is greater
-- than the corresponding @m@ or lower than @n@ in the @'Natural' n m@ type.
reifyToNatural :: forall n m . (KnownNat n, KnownNat m) => Int -> Natural n m
reifyToNatural i =
  let start = fromInteger (natVal (Proxy :: Proxy n))
      end   = fromInteger (natVal (Proxy :: Proxy m))
   in if start <= i && i <= end
        then Nat i
        else error "Off limits"

-- | Auxiliary function that promotes binary 'Int' functions to 'Natural' binary
-- functions.
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
       in reifyToNatural (start + i)
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
newtype List a = L [a]
  deriving (Eq, Show, Read)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x:ps | ps <- powerset xs]

instance
  ( Enum a,
    Bounded a
  ) => Bounded (List a)
  where
  minBound = L []
  maxBound = L [minBound .. maxBound]

instance
  ( Bounded a,
    Enum a,
    Eq a
  ) => Enum (List a)
  where
  toEnum i =
    let as = [minBound .. maxBound]
        in L (powerset as !! i)

  fromEnum (L []) = 0
  fromEnum (L x) =
    let as = [minBound .. maxBound]
        in fromMaybe (error "Does not exist") $ elemIndex x (powerset as)

-- | Constrained category instance
class Category k where
  type Object k o :: Constraint
  type Object k o = ()
  id :: Object k a => k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)
