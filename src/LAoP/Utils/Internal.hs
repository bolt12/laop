{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module     : LAoP.Utils.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Bounded index types ('Ranged', 'BoundedList') and the constrained 'Category'
class. "LAoP.Utils" is the public interface; this module also exposes the raw
'Ranged' constructor.

This is an Internal module and it is not supposed to be imported.
-}
module LAoP.Utils.Internal (
  -- * 'Ranged' data type
  Ranged (UnsafeRanged),
  pattern Rng,
  mkRanged,

  -- * Coerce auxiliary functions to help promote 'Int' typed functions to
  -- 'Ranged' typed functions.
  coerceRanged,
  coerceRanged2,
  coerceRanged3,

  -- * Deprecated aliases
  Natural,
  reifyToNatural,
  coerceNat,
  coerceNat2,
  coerceNat3,

  -- * 'BoundedList' data type
  BoundedList (..),

  -- * Category type class
  Category (..),
)
where

import           Control.DeepSeq
import           Data.Bits       (bit, testBit, (.|.))
import           Data.Coerce
import           Data.Kind
import           Data.Proxy
import           GHC.Enum        (boundedEnumFrom, boundedEnumFromThen)
import           GHC.Read        (expectP, parens)
import           GHC.Stack       (HasCallStack)
import           GHC.TypeLits    hiding (Natural)
import           Prelude         hiding (id, (.))
import qualified Prelude
import           Text.Read       (Lexeme (Ident), Read (..), pfail, prec, step)

{- | Wrapper around 'Int's that have a restrictive semantic associated.
A value of type @'Ranged' n m@ can only be instantiated with some 'Int'
@i@ where @n <= i <= m@.

Build values with 'mkRanged', numeric literals or 'toEnum'; read them with the
'Rng' pattern. The raw constructor is not exported from "LAoP.Utils", and the
nominal roles stop 'coerce' from moving a value into other bounds.
-}
newtype Ranged (start :: Nat) (end :: Nat) = UnsafeRanged Int
  deriving (Eq, Ord, NFData)

type role Ranged nominal nominal

-- | Matches a 'Ranged' value and exposes its 'Int'. Matching only.
pattern Rng :: Int -> Ranged n m
pattern Rng i <- UnsafeRanged i

{-# COMPLETE Rng #-}

instance Show (Ranged n m) where
  showsPrec d (UnsafeRanged i) = showParen (d > 10) (showString "Rng " . showsPrec 11 i)

-- | Reads what 'show' prints, failing on values outside the range.
instance (KnownNat n, KnownNat m) => Read (Ranged n m) where
  readPrec = parens . prec 10 $ do
    expectP (Ident "Rng")
    i <- step readPrec
    if inRange @n @m (toInteger i) then pure (UnsafeRanged i) else pfail

inRange :: forall n m. (KnownNat n, KnownNat m) => Integer -> Bool
inRange i = natVal (Proxy @n) <= i && i <= natVal (Proxy @m)

outOfRange :: forall n m a. (HasCallStack, KnownNat n, KnownNat m) => String -> Integer -> a
outOfRange what i =
  error $
    what
      ++ ": "
      ++ show i
      ++ " is outside ["
      ++ show (natVal (Proxy @n))
      ++ ", "
      ++ show (natVal (Proxy @m))
      ++ "]"

{- | Throws a runtime error if any of the operations overflows or
underflows.
-}
instance (KnownNat n, KnownNat m) => Num (Ranged n m) where
  (Rng a) + (Rng b) = mkRanged @n @m (a + b)
  (Rng a) - (Rng b) = mkRanged @n @m (a - b)
  (Rng a) * (Rng b) = mkRanged @n @m (a * b)
  abs (Rng a) = mkRanged @n @m (abs a)
  signum (Rng a) = mkRanged @n @m (signum a)
  fromInteger i
    | inRange @n @m i = UnsafeRanged (fromInteger i)
    | otherwise = outOfRange @n @m "Ranged.fromInteger" i

{- | Ranged constructor function. Throws a runtime error if the 'Int' value
is greater than @m@ or lower than @n@ in the @'Ranged' n m@ type.
-}
mkRanged :: forall n m. (HasCallStack, KnownNat n, KnownNat m) => Int -> Ranged n m
mkRanged i
  | inRange @n @m (toInteger i) = UnsafeRanged i
  | otherwise = outOfRange @n @m "mkRanged" (toInteger i)

{- | Promotes binary 'Int' functions to 'Ranged' binary functions. Throws a
runtime error if the result falls outside the result range.
-}
coerceRanged ::
  (HasCallStack, KnownNat c, KnownNat c') =>
  (Int -> Int -> Int) ->
  (Ranged a a' -> Ranged b b' -> Ranged c c')
coerceRanged f (Rng x) (Rng y) = mkRanged (f x y)

{- | Promotes ternary (binary) 'Int' functions to 'Ranged' functions. Throws a
runtime error if the result falls outside the result range.
-}
coerceRanged2 ::
  (HasCallStack, KnownNat d, KnownNat d') =>
  ((Int, Int) -> Int -> Int) ->
  ((Ranged a a', Ranged b b') -> Ranged c c' -> Ranged d d')
coerceRanged2 f (Rng x, Rng y) (Rng z) = mkRanged (f (x, y) z)

-- | Promotes binary 'Int' functions to 'Ranged' functions with polymorphic result.
coerceRanged3 :: (Int -> Int -> a) -> (Ranged b b' -> Ranged c c' -> a)
coerceRanged3 = coerce

instance (KnownNat n, KnownNat m) => Bounded (Ranged n m) where
  minBound = UnsafeRanged (fromInteger (natVal (Proxy :: Proxy n)))
  maxBound = UnsafeRanged (fromInteger (natVal (Proxy :: Proxy m)))

{- | Enumerates the range from 0: @'toEnum' 0 == 'minBound'@. @[x ..]@ and
@[x, y ..]@ stop at the bounds.
-}
instance (KnownNat n, KnownNat m) => Enum (Ranged n m) where
  toEnum i =
    let start = fromInteger (natVal (Proxy :: Proxy n))
     in mkRanged (start + i)

  fromEnum (Rng val) = val - fromInteger (natVal (Proxy :: Proxy n))

  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

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

  fromEnum (Left a)  = fromEnum a
  fromEnum (Right b) = fromEnum (maxBound :: a) + fromEnum b + 1

{- | A subset of a finite type, represented by the list of its members.

Order and duplicates in the list carry no meaning. The 'Enum' instance numbers
subsets by a bitmask in which the first value of the element type is the most
significant bit. For a two-value type @{a0, a1}@ the order is @L []@,
@L [a1]@, @L [a0]@, @L [a0, a1]@.
-}
newtype BoundedList a = L [a]
  deriving (Eq, Show, Read)

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

{- | Constrained category class.

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

-- Deprecated aliases

-- | Deprecated alias of 'Ranged'.
type Natural = Ranged
{-# DEPRECATED Natural "Use Ranged instead" #-}

-- | Deprecated alias of 'mkRanged'.
reifyToNatural :: forall n m. (HasCallStack, KnownNat n, KnownNat m) => Int -> Ranged n m
reifyToNatural = mkRanged
{-# DEPRECATED reifyToNatural "Use mkRanged instead" #-}

-- | Deprecated alias of 'coerceRanged'.
coerceNat :: (HasCallStack, KnownNat c, KnownNat c') => (Int -> Int -> Int) -> (Ranged a a' -> Ranged b b' -> Ranged c c')
coerceNat = coerceRanged
{-# DEPRECATED coerceNat "Use coerceRanged instead" #-}

-- | Deprecated alias of 'coerceRanged2'.
coerceNat2 :: (HasCallStack, KnownNat d, KnownNat d') => ((Int, Int) -> Int -> Int) -> ((Ranged a a', Ranged b b') -> Ranged c c' -> Ranged d d')
coerceNat2 = coerceRanged2
{-# DEPRECATED coerceNat2 "Use coerceRanged2 instead" #-}

-- | Deprecated alias of 'coerceRanged3'.
coerceNat3 :: (Int -> Int -> a) -> (Ranged b b' -> Ranged c c' -> a)
coerceNat3 = coerceRanged3
{-# DEPRECATED coerceNat3 "Use coerceRanged3 instead" #-}
