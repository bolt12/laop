{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Dist.Internal
        ( 
        Dist(..),
        Prob,
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

import Matrix.Nat
import Utils
import GHC.TypeLits
import Data.Proxy
import Data.List (sortBy)

-- | Type synonym for probability value
type Prob = Double

-- | Type synonym for column vector matrices. This represents a probability
-- distribution.
type Dist a (m :: Nat) = (FromLists Prob () (FromNat m)) => Matrix Prob 1 m

-- | Constructs a Bernoulli distribution
choose :: Prob -> Dist a 2
choose prob = col [prob, 1 - prob]

-- | Creates a distribution given a shape function
shape :: (Prob -> Prob) -> [a] -> Dist a m
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = map f (iterate (+incr) 0)
   in  fromFreqs (zip xs ps)

-- | Constructs a Linear distribution
linear :: [a] -> Dist a m
linear = shape id

-- | Constructs an Uniform distribution
uniform :: [a] -> Dist a m
uniform = shape (const 1)

-- | Constructs an Negative Exponential distribution
negExp :: [a] -> Dist a m
negExp = shape (\x -> exp (-x))

-- | Constructs an Normal distribution
normal :: [a] -> Dist a m
normal = shape (normalCurve 0.5 0.5)

-- | Transforms a 'Dist' into a list of pairs.
toValues :: forall a m . (Enum a, KnownNat m, FromLists Prob () (FromNat m)) => Dist a m -> [(a, Prob)]
toValues d =
    let rows = fromInteger (natVal (Proxy :: Proxy m))
        probs = toList d
        res = zip (map toEnum [0..rows]) probs
     in res

-- | Pretty a distribution
prettyDist :: forall a m . (Show a, Enum a, KnownNat m, FromLists Prob () (FromNat m)) => Dist a m -> String
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
prettyPrintDist :: forall a m . (Show a, Enum a, KnownNat m, FromLists Prob () (FromNat m)) => Dist a m -> IO ()
prettyPrintDist = putStrLn . prettyDist @a

-- Auxiliary functions

fromFreqs :: [(a,Prob)] -> Dist a m
fromFreqs xs = col (map (\(x,p) -> p/q) xs)
           where q = sum $ map snd xs

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
