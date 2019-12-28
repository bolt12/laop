{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Dist.Internal
        ( 
        Dist(..),
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

type Prob = Double
type Dist a m = (FromLists Prob () (FromNat m)) => Matrix Prob 1 m

choose :: Prob -> Dist a 2
choose prob = col [prob, 1 - prob]

shape :: (Prob -> Prob) -> [a] -> Dist a m
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = map f (iterate (+incr) 0)
   in  fromFreqs (zip xs ps)

linear :: [a] -> Dist a m
linear = shape id

uniform :: [a] -> Dist a m
uniform = shape (const 1)

negExp :: [a] -> Dist a m
negExp = shape (\x -> exp (-x))

normal :: [a] -> Dist a m
normal = shape (normalCurve 0.5 0.5)

---

fromFreqs :: [(a,Prob)] -> Dist a m
fromFreqs xs = col (map (\(x,p) -> p/q) xs)
           where q = sum $ map snd xs

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
