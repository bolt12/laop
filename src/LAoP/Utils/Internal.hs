{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LAoP.Utils.Internal (
  -- * 'Natural' data type
  Natural (..),
  reifyToNatural,

  -- * Coerce auxiliar functions to help promote 'Int' typed functions to

  -- 'Natural' typed functions.
  coerceNat,
  coerceNat2,
  coerceNat3,

  -- * 'BoundedList' data type
  BoundedList (..),

  -- * Category type class
  Category (..),
)
where

import Control.DeepSeq
import Data.Bits (bit, testBit, (.|.))
import Data.Coerce
import Data.Kind
import Data.Proxy
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import GHC.Generics
import GHC.TypeLits hiding (Natural)
import Prelude hiding (id, (.))
import Prelude qualified

{- | Wrapper around 'Int's that have a restrictive semantic associated.
A value of type @'Natural' n m@ can only be instanciated with some 'Int'
@i@ that's @n <= i <= m@.
-}
newtype Natural (start :: Nat) (end :: Nat) = Nat Int
  deriving (Show, Read, Eq, Ord, NFData, Generic)

{- | Throws a runtime error if any of the operations overflows or
underflows.
-}
instance (KnownNat n, KnownNat m) => Num (Natural n m) where
  (Nat a) + (Nat b) = reifyToNatural @n @m (a + b)
  (Nat a) - (Nat b) = reifyToNatural @n @m (a - b)
  (Nat a) * (Nat b) = reifyToNatural @n @m (a * b)
  abs (Nat a) = reifyToNatural @n @m (abs a)
  signum (Nat a) = reifyToNatural @n @m (signum a)
  fromInteger i = reifyToNatural @n @m (fromInteger i)

{- | Natural constructor function. Throws a runtime error if the 'Int' value is greater
than the corresponding @m@ or lower than @n@ in the @'Natural' n m@ type.
-}
reifyToNatural :: forall n m. (KnownNat n, KnownNat m) => Int -> Natural n m
reifyToNatural i =
  let start = fromInteger (natVal (Proxy :: Proxy n))
      end = fromInteger (natVal (Proxy :: Proxy m))
   in if start <= i && i <= end
        then Nat i
        else error "Off limits"

{- | Auxiliary function that promotes binary 'Int' functions to 'Natural' binary
functions.
-}
coerceNat :: (Int -> Int -> Int) -> (Natural a a' -> Natural b b' -> Natural c c')
coerceNat = coerce

{- | Auxiliary function that promotes ternary (binary) 'Int' functions to 'Natural'
functions.
-}
coerceNat2 :: ((Int, Int) -> Int -> Int) -> ((Natural a a', Natural b b') -> Natural c c' -> Natural d d')
coerceNat2 = coerce

{- | Auxiliary function that promotes ternary (binary) 'Int' functions to 'Natural'
functions.
-}
coerceNat3 :: (Int -> Int -> a) -> (Natural b b' -> Natural c c' -> a)
coerceNat3 = coerce

instance (KnownNat n, KnownNat m) => Bounded (Natural n m) where
  minBound = Nat $ fromInteger (natVal (Proxy :: Proxy n))
  maxBound = Nat $ fromInteger (natVal (Proxy :: Proxy m))

instance (KnownNat n, KnownNat m) => Enum (Natural n m) where
  toEnum i =
    let start = fromInteger (natVal (Proxy :: Proxy n))
     in reifyToNatural (start + i)

  -- \| Throws a runtime error if the value is off limits
  fromEnum (Nat nat) =
    let start = fromInteger (natVal (Proxy :: Proxy n))
        end = fromInteger (natVal (Proxy :: Proxy m))
     in if start <= nat && nat <= end
          then nat - start
          else error "Off limits"

{- | Optimized 'Enum' instance for tuples that comply with the given
constraints.
-}
instance
  ( Enum a
  , Enum b
  , Bounded b
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
  ( Bounded a
  , Bounded b
  ) =>
  Bounded (Either a b)
  where
  minBound = Left (minBound :: a)
  maxBound = Right (maxBound :: b)

instance
  ( Enum a
  , Bounded a
  , Enum b
  , Bounded b
  ) =>
  Enum (Either a b)
  where
  toEnum i =
    let la = fmap Left ([minBound .. maxBound] :: [a])
        lb = fmap Right ([minBound .. maxBound] :: [b])
     in (la ++ lb) !! i

  fromEnum (Left a) = fromEnum a
  fromEnum (Right b) = fromEnum (maxBound :: a) + fromEnum b + 1

{- | A subset of a finite type, represented by the list of its members.

Order and duplicates in the list carry no meaning: '==' compares the subsets,
so @L [True, False] == L [False, True, True]@. 'Show' and 'Read' keep the list
as written. The 'Enum' instance numbers subsets by a bitmask in which the first
value of the element type is the most significant bit. For a two-value type
@{a0, a1}@ the order is @L []@, @L [a1]@, @L [a0]@, @L [a0, a1]@.
-}
newtype BoundedList a = L [a]
  deriving (Show, Read)

-- | Subset equality: the same members, in any order and with any repetition.
instance (Eq a) => Eq (BoundedList a) where
  L xs == L ys = all (`elem` ys) xs && all (`elem` xs) ys

instance (Enum a, Bounded a) => Bounded (BoundedList a) where
  minBound = L []
  maxBound = L [minBound .. maxBound]

instance (Bounded a, Enum a) => Enum (BoundedList a) where
  toEnum i
    | 0 <= i && i < bit n = L [x | (j, x) <- zip [0 ..] universe, testBit i (n - 1 - j)]
    | otherwise =
        error ("BoundedList.toEnum: " ++ show i ++ " is outside [0, " ++ show (bit n :: Int) ++ ")")
    where
      universe = [minBound .. maxBound] :: [a]
      n = length universe

  fromEnum (L xs) = foldl' (.|.) 0 [bit (n - 1 - (fromEnum x - lo)) | x <- xs]
    where
      n = length ([minBound .. maxBound] :: [a])
      lo = fromEnum (minBound :: a)

  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

infixr 9 .

{- | A category whose objects can be constrained. For matrices 'Object' says
which types can index a dimension; for functions there is no constraint.

@.@ is right-associative, like 'Prelude..', so a chain @f . g . v@ applied to a
vector @v@ multiplies matrix by vector twice rather than first forming @f . g@.
-}
class Category (k :: j -> j -> Type) where
  type Object k (o :: j) :: Constraint
  type Object k o = ()
  id :: (Object k a) => k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)
