{-# LANGUAGE FlexibleContexts #-}
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

sumSS :: Natural 6 -> Natural 6 -> Natural 12
sumSS = coerceNat (+)

mulSS :: Natural 6 -> Natural 6 -> Natural 36
mulSS = coerceNat (*)

sumSSM = fromF' (uncurry sumSS)

mulSSM3 = fromF' (mulSS (nat 3))

die :: Dist (Natural 6) 6
die = uniform [nat @6 1 .. nat 6]

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
    putStrLn "\n Checking that the last result is indeed a distribution: "
    prettyPrint (bang `comp` sumSSM `comp` khatri die die)
    putStrLn "\n Probability of grass being wet:"
    prettyPrint (grass_wet `comp` state)
    putStrLn "\n Probability of rain:"
    prettyPrint (rainning `comp` state)
