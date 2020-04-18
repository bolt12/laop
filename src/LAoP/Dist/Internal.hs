{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module LAoP.Dist.Internal
        (
        Dist(..),
        Prob,

        Countable,
        CountableN,
        CountableDimensionsN,
        FromListsN,
        Liftable,
        TrivialP,

        fmapD,
        unitD,
        multD,
        selectD,
        branchD,
        ifD,
        returnD,
        bindD,
        (??),

        choose,
        shape,
        linear,
        uniform,
        negExp,
        normal,
        toValues,
        prettyDist,
        prettyPrintDist
        )
    where

import LAoP.Matrix.Type hiding (TrivialP, Countable, CountableDimensions, CountableN, CountableDimensionsN, FromListsN)
import Prelude hiding (id, (.))
import qualified LAoP.Matrix.Alternative as I
import LAoP.Utils
import GHC.TypeLits
import Data.Proxy
import Data.List (sortBy)
import Control.DeepSeq
import Data.Bool

-- | Type synonym for probability value
type Prob = Double

-- | Type synonym for column vector matrices. This represents a probability
-- distribution.
newtype Dist a = D (Matrix Prob () a)
  deriving (Show, Num, Eq, Ord, NFData) via (Matrix Prob () a)

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a              = KnownNat (I.Count a)
type CountableN a             = KnownNat (I.Count (I.Normalize a))
type CountableDimensionsN a b = (CountableN a, CountableN b)
type FromListsN a b           = I.FromLists Prob (I.Normalize a) (I.Normalize b)
type TrivialP a b             = Normalize (a, b) ~ Normalize (Normalize a, Normalize b)

-- | Functor instance
fmapD :: Liftable a b => (a -> b) -> Dist a -> Dist b
fmapD f (D m) = D (fromF f `comp` m)

-- | Applicative/Monoidal instance 'unit' function
unitD :: Dist ()
unitD = D (one 1)

-- | Applicative/Monoidal instance 'mult' function
multD :: (FstM a b, SndM a b) => Dist a -> Dist b -> Dist (a, b)
multD (D a) (D b) = D (kr a b)

-- | Selective instance function
selectD ::
       ( EqN a,
         EqN b,
         EnumerableN b,
         ConstructN b
       ) => Dist (Either a b) -> Matrix Prob a b -> Dist b
selectD (D d) m = D (selectM d m)

-- | Chooses which of the two given effectful
-- functions to apply to a given argument; 
branchD ::
       ( Num e,
         EqN a,
         EqN b,
         EqN c,
         ConstructableN a b,
         ConstructN c,
         EnumerableN a,
         EnumerableN b,
         EnumerableN c
       )
       => Dist (Either a b) -> Matrix Prob a c -> Matrix Prob b c -> Dist c
branchD x l r = f x `selectD` g l `selectD` r
  where
    f (D m) = D (fork (tr i1) (i1 `comp` tr i2) `comp` m)
    g m = i2 `comp` m

-- | Branch on a Boolean value, skipping unnecessary computations.
ifD ::
    ( Num e,
      EqN a,
      ConstructN a,
      EnumerableN a
    )
    => Dist Bool -> Dist a -> Dist a -> Dist a
ifD x (D t) (D e) = branchD x' t e
  where
    x' = bool (Right ()) (Left ()) `fmapD` x

-- | Monad instance 'return' function
returnD :: Liftable () a => a -> Dist a
returnD = D . returnM

-- | Monad instance '(>>=)' function
bindD :: Dist a -> Matrix Prob a b -> Dist b
bindD (D d) m = D (m `comp` d)

-- | Extract probabilities given an Event.
(??) :: 
     ( Enum a, 
       Countable a,
       FromListsN () a
     ) => (a -> Bool) -> Dist a -> Prob
(??) p d =
    let l = toValues d
        x = filter (p . fst) l
     in sum . map snd $ x

-- Distribution Construction

-- | Constructs a Bernoulli distribution
choose :: (FromListsN () a) => Prob -> Dist a
choose prob = D (colL [prob, 1 - prob])

-- | Creates a distribution given a shape function
shape :: (FromListsN () a) => (Prob -> Prob) -> [a] -> Dist a
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = map f (iterate (+incr) 0)
   in  fromFreqs (zip xs ps)

-- | Constructs a Linear distribution
linear :: (FromListsN () a) => [a] -> Dist a
linear = shape id

-- | Constructs an Uniform distribution
uniform :: (FromListsN () a) => [a] -> Dist a
uniform = shape (const 1)

-- | Constructs an Negative Exponential distribution
negExp :: (FromListsN () a) => [a] -> Dist a
negExp = shape (\x -> exp (-x))

-- | Constructs an Normal distribution
normal :: (FromListsN () a) => [a] -> Dist a
normal = shape (normalCurve 0.5 0.5)

-- | Transforms a 'Dist' into a list of pairs.
toValues :: forall a . (Enum a, Countable a, FromListsN () a) => Dist a -> [(a, Prob)]
toValues (D d) =
    let rows = fromInteger (natVal (Proxy :: Proxy (Count a)))
        probs = toList d
        res = zip (map toEnum [0..rows]) probs
     in res

-- | Pretty a distribution
prettyDist :: forall a. (Show a, Enum a, Countable a, FromListsN () a) => Dist a -> String
prettyDist d =
    let values = sortBy (\(a, p1) (b, p2) -> compare p2 p1) (toValues @a d)
        w = maximum (map (length . show . fst) values)
     in concatMap
          (\(x,p) -> showR w x ++ ' ': showProb p ++ "\n")
          values
  where
    showProb p = show (p * 100) ++ "%"
    showR n x = show x ++ " " 

-- | Pretty Print a distribution
prettyPrintDist :: forall a . (Show a, Enum a, Countable a, FromListsN () a) => Dist a -> IO ()
prettyPrintDist = putStrLn . prettyDist @a

-- Auxiliary functions

fromFreqs :: (FromListsN () a) => [(a,Prob)] -> Dist a
fromFreqs xs = D (colL (map (\(x,p) -> p/q) xs))
           where q = sum $ map snd xs

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
