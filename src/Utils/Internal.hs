{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Internal
    ( Natural(..),
      nat,
      coerceNat,
      coerceNat2
    )
    where

import GHC.TypeLits
import Data.Proxy
import Data.Coerce

newtype Natural (nat :: Nat) = Nat Int
  deriving (Show, Read, Eq, Num)

nat :: forall n . (KnownNat n) => Int -> Natural n
nat i = let nat = fromInteger (natVal (Proxy :: Proxy n))
         in if i <= nat
               then Nat i
               else error "Off limits"

coerceNat :: (Int -> Int -> Int) -> (Natural a -> Natural b -> Natural c)
coerceNat = coerce

coerceNat2 :: ((Int, Int) -> Int -> Int) -> ((Natural a, Natural b) -> Natural c -> Natural d)
coerceNat2 = coerce

instance KnownNat n => Bounded (Natural n) where
    minBound = Nat 0
    maxBound = Nat $ fromInteger (natVal (Proxy :: Proxy n))

instance KnownNat n => Enum (Natural n) where
    toEnum = nat
    fromEnum (Nat nat) = let val = fromInteger (natVal (Proxy :: Proxy n))
                          in if nat > val
                                then error "Off limits"
                                else nat

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
