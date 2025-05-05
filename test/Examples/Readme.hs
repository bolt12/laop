{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Readme (
  exec,
)
where

import GHC.Generics qualified as G
import LAoP.Matrix.Type
import LAoP.Relation qualified as R
import LAoP.Utils
import Prelude hiding (id, (.))

-- Monty Hall Problem
data Outcome = Win | Lose
  deriving (Bounded, Enum, Eq, Show, G.Generic)

switch :: Outcome -> Outcome
switch Win = Lose
switch Lose = Win

firstChoice :: Matrix Double () Outcome
firstChoice = col [1 / 3, 2 / 3]

secondChoice :: Matrix Double Outcome Outcome
secondChoice = fromF switch

-- Dice sum

type SS = Natural 1 6 -- Sample Space

sumSS :: SS -> SS -> Natural 2 12
sumSS = coerceNat (+)

sumSSM :: Matrix Double (SS, SS) (Natural 2 12)
sumSSM = fromF (uncurry sumSS)

condition :: (Int, Int) -> Int -> Int
condition (a, b) c =
  if a == b
    then a * 3
    else a + b + c

conditionSS :: (SS, SS) -> SS -> Natural 3 18
conditionSS = coerceNat2 condition

conditionalThrows :: Matrix Double () (Natural 3 18)
conditionalThrows = fromF (uncurry conditionSS) . kr (kr die die) die

die :: Matrix Double () SS
die = col $ map (const (1 / 6)) [reifyToNatural @1 @6 1 .. reifyToNatural 6]

-- Sprinkler
data G = Dry | Wet
  deriving (Bounded, Enum, Eq, Show, G.Generic)

data S = Off | On
  deriving (Bounded, Enum, Eq, Show, G.Generic)

data R = No | Yes
  deriving (Bounded, Enum, Eq, Show, G.Generic)

rain :: Matrix Double () R
rain = matrixBuilder gen
  where
    gen (_, No) = 0.8
    gen (_, Yes) = 0.2

sprinkler :: Matrix Double R S
sprinkler = matrixBuilder gen
  where
    gen (No, Off) = 0.6
    gen (No, On) = 0.4
    gen (Yes, Off) = 0.99
    gen (Yes, On) = 0.01

grass :: Matrix Double (S, R) G
grass = matrixBuilder gen
  where
    gen ((Off, No), Dry) = 1
    gen ((Off, Yes), Dry) = 0.2
    gen ((On, No), Dry) = 0.1
    gen ((On, Yes), Dry) = 0.01
    gen ((Off, No), Wet) = 0
    gen ((Off, Yes), Wet) = 0.8
    gen ((On, No), Wet) = 0.9
    gen ((On, Yes), Wet) = 0.99

tag f = kr f id

state g s r = tag g . tag s . r

grass_wet :: Matrix Double () (G, (S, R)) -> Matrix Double One One
grass_wet s = row [0, 1] . fstM . s

rainning :: Matrix Double (G, (S, R)) One
rainning = row [0, 1] . sndM . sndM

-- Alcuin Puzzle

data Being = Farmer | Fox | Goose | Beans
  deriving (Bounded, Enum, Eq, Show, G.Generic)

data Bank = LeftB | RightB
  deriving (Bounded, Enum, Eq, Show, G.Generic)

eats :: Being -> Being -> Bool
eats Fox Goose = True
eats Goose Beans = True
eats _ _ = False

eatsR :: R.Relation Being Being
eatsR = R.toRel eats

cross :: Bank -> Bank
cross LeftB = RightB
cross RightB = LeftB

crossR :: R.Relation Bank Bank
crossR = R.fromF cross

-- | Initial state, everyone in the left bank
locationLeft :: Being -> Bank
locationLeft _ = LeftB

locationLeftR :: R.Relation Being Bank
locationLeftR = R.fromF locationLeft

-- | Initial state, everyone in the right bank
locationRight :: Being -> Bank
locationRight _ = RightB

locationRightR :: R.Relation Being Bank
locationRightR = R.fromF locationRight

-- Properties

-- Being at the same bank
sameBank :: R.Relation Being Bank -> R.Relation Being Being
sameBank = R.ker

-- Risk of somebody eating somebody else
canEat :: R.Relation Being Bank -> R.Relation Being Being
canEat w = sameBank w `R.intersection` eatsR

-- "Starvation" property.
inv :: R.Relation Being Bank -> Bool
inv w = (w `R.comp` canEat w) `R.sse` (w `R.comp` farmer)
  where
    farmer :: R.Relation Being Being
    farmer = R.fromF (const Farmer)

-- Arbitrary state
bankState :: Being -> Bank -> Bool
bankState Farmer LeftB = True
bankState Fox LeftB = True
bankState Goose RightB = True
bankState Beans RightB = True
bankState _ _ = False

bankStateR :: R.Relation Being Bank
bankStateR = R.toRel bankState

-- Main
exec :: IO ()
exec = do
  putStrLn "Monty Hall Problem solution:"
  prettyPrint (secondChoice . firstChoice)
  putStrLn "\n Sum of dices probability:"
  prettyPrint (sumSSM . kr die die)
  putStrLn "\n Conditional dice throw:"
  prettyPrint conditionalThrows
  putStrLn "\n Checking that the last result is indeed a distribution: "
  prettyPrint (bang . sumSSM . kr die die)
  putStrLn "\n Probability of grass being wet:"
  prettyPrint (grass_wet (state grass sprinkler rain))
  putStrLn "\n Probability of rain:"
  prettyPrint (rainning . state grass sprinkler rain)
  putStrLn "\n Is the arbitrary state a valid state? (Alcuin Puzzle)"
  print (inv bankStateR)
