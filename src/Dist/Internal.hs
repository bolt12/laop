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
        normal
        )
    where

import Matrix.Nat
import Utils

-- | Type synonym for probability value
type Prob = Double

-- | Type synonym for column vector matrices. This represents a probability
-- distribution.
type Dist a m = (FromLists Prob () (FromNat m)) => Matrix Prob 1 m

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

-- Auxiliary functions

fromFreqs :: [(a,Prob)] -> Dist a m
fromFreqs xs = col (map (\(x,p) -> p/q) xs)
           where q = sum $ map snd xs

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
