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

firstChoice :: Dist Outcome 2
firstChoice = choose (1/3)

secondChoice :: Matrix Double 2 2
secondChoice = fromF switch 

-- Dice sum

sumSS :: Natural 6 -> Natural 6 -> Natural 12
sumSS = coerceNat (+) 

sumSSM :: Matrix Double 36 12
sumSSM = fromF (uncurry sumSS)

die :: Dist (Natural 6) 6
die = uniform [(nat 1)..(nat @6 6)]

main :: IO ()
main = do 
    prettyPrint (p1 @Double @1 `comp` secondChoice `comp` firstChoice)
    prettyPrint (sumSSM `comp` (die >< die))
    prettyPrint (bang `comp` sumSSM `comp` (die >< die))

