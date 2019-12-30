{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Matrix.Type
import Utils
import Dist
import GHC.TypeLits
import Data.Coerce
import GHC.Generics
import Control.Category hiding (id)
import Prelude hiding ((.))

-- Monty Hall Problem
data Outcome = Win | Lose
    deriving (Bounded, Enum, Eq, Show, Generic)

switch :: Outcome -> Outcome
switch Win = Lose
switch Lose = Win

firstChoice :: Dist Outcome
firstChoice = col [1/3, 2/3]

secondChoice :: Matrix Double Outcome Outcome
secondChoice = fromF' switch 

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

conditionalThrows = fromF' (uncurry conditionSS) . khatri (khatri die die) die

die :: Dist SS
die = uniform [nat @1 @6 1 .. nat 6]

-- Sprinkler

rain :: Dist Bool
rain = choose 0.8

sprinkler :: Matrix Double Bool Bool
sprinkler = fromLists [[0.6, 0.99], [0.4, 0.01]]

grass :: Matrix Double (Bool, Bool) Bool
grass = fromLists [[1, 0.2, 0.1, 0.01], [0, 0.8, 0.9, 0.99]]

state :: Dist (Bool, (Bool, Bool))
state = khatri grass identity . khatri sprinkler identity . rain

grass_wet :: Matrix Double (Bool, (Bool, Bool)) One
grass_wet = row [0,1] . kp1

rainning :: Matrix Double (Bool, (Bool, Bool)) One
rainning = row [0,1] . kp2 . kp2 

main :: IO ()
main = do
    -- putStrLn "Monty Hall Problem solution:"
    -- prettyPrint (secondChoice . firstChoice)
    -- putStrLn "\n Sum of dices probability:"
    -- prettyPrint (sumSSM `comp` khatri die die)
    -- prettyPrintDist @(Natural 2 12) (sumSSM . khatri die die)
    -- putStrLn "\n Conditional dice throw:"
    -- prettyPrintDist @(Natural 3 18) conditionalThrows
    -- putStrLn "\n Checking that the last result is indeed a distribution: "
    -- prettyPrint (bang . sumSSM . khatri die die)
    putStrLn "\n Probability of grass being wet:"
    prettyPrint (grass_wet . state)
    -- putStrLn "\n Probability of rain:"
    -- prettyPrint (rainning . state)
