{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Matrix.Nat
import Utils
import Dist
import GHC.TypeLits
import Data.Coerce

-- Monty Hall Problem
data Outcome = Win | Lose
    deriving (Bounded, Enum, Eq, Show)

switch :: Outcome -> Outcome
switch Win = Lose
switch Lose = Win

firstChoice :: Dist Outcome m
firstChoice = col [1/3, 2/3]

secondChoice :: Matrix Double 2 2
secondChoice = fromF switch 

-- Dice sum

type SS = Natural 1 6

sumSS :: SS -> SS -> Natural 2 12
sumSS = coerceNat (+)

sumSSM = fromF' (uncurry sumSS)

condition :: (Int, Int) -> Int -> Int
condition (fst, snd) thrd = if fst == snd
                               then fst * 3
                               else fst + snd + thrd

conditionSS :: (SS, SS) -> SS -> Natural 3 18
conditionSS = coerceNat2 condition

conditionalThrows = fromF' (uncurry conditionSS) `comp` khatri die (khatri die die)

die :: Dist (Natural 1 6) 6
die = uniform [nat @1 @6 1 .. nat 6]

-- Sprinkler

rain :: Dist Bool 2
rain = choose 0.8

sprinkler :: Matrix Double 2 2
sprinkler = fromLists [[0.6, 0.99], [0.4, 0.01]]

grass :: Matrix Double 4 2
grass = fromLists [[1, 0.2, 0.1, 0.01], [0, 0.8, 0.9, 0.99]]

state = khatri grass identity `comp` khatri sprinkler identity `comp` rain

grass_wet = row [0,1] `comp` kp1 @Double @2 @4

rainning = row [0,1] `comp` kp2 @Double @2 @2 `comp` kp2 @Double @2 @4

main :: IO ()
main = do
    putStrLn "Monty Hall Problem solution:"
    prettyPrint (p1 @Double @1 `comp` secondChoice `comp` firstChoice)
    putStrLn "\n Sum of dices probability:"
    prettyPrint (sumSSM `comp` khatri die die)
    prettyPrintDist @(Natural 2 12) (sumSSM `comp` khatri die die)
    putStrLn "\n Conditional dice throw:"
    prettyPrintDist @(Natural 3 18) conditionalThrows
    putStrLn "\n Checking that the last result is indeed a distribution: "
    prettyPrint (bang `comp` sumSSM `comp` khatri die die)
    putStrLn "\n Probability of grass being wet:"
    prettyPrint (grass_wet `comp` state)
    putStrLn "\n Probability of rain:"
    prettyPrint (rainning `comp` state)
