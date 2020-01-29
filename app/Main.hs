{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import LAoP.Matrix.Type
import qualified LAoP.Relation as R
import LAoP.Utils
import LAoP.Dist
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

firstChoice :: Matrix Double () Outcome
firstChoice = col [1/3, 2/3]

secondChoice :: Matrix Double Outcome Outcome
secondChoice = fromF' switch 

-- Dice sum

type SS = Natural 1 6 -- Sample Space

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

die :: Matrix Double () SS
die = col $ map (const (1/6)) [nat @1 @6 1 .. nat 6]

-- Sprinkler

rain :: Matrix Double () Bool
rain = col [0.8, 0.2]

sprinkler :: Matrix Double Bool Bool
sprinkler = fromLists [[0.6, 0.99], [0.4, 0.01]]

grass :: Matrix Double (Bool, Bool) Bool
grass = fromLists [[1, 0.2, 0.1, 0.01], [0, 0.8, 0.9, 0.99]]

state :: Matrix Double () (Bool, (Bool, Bool))
state = khatri grass identity . khatri sprinkler identity . rain

grass_wet :: Matrix Double (Bool, (Bool, Bool)) One
grass_wet = row [0,1] . kp1

rainning :: Matrix Double (Bool, (Bool, Bool)) One
rainning = row [0,1] . kp2 . kp2 

-- Alcuin Puzzle

data Being = Farmer | Fox | Goose | Beans
  deriving (Bounded, Enum, Eq, Show, Generic)

data Bank = LeftB | RightB
  deriving (Bounded, Enum, Eq, Show, Generic)

eats :: Being -> Being -> Bool
eats Fox Goose   = True
eats Goose Beans = True
eats _ _         = False

eatsR :: R.Relation Being Being
eatsR = R.toRel eats

cross :: Bank -> Bank
cross LeftB = RightB
cross RightB = LeftB

crossR :: R.Relation Bank Bank
crossR = R.fromF' cross

-- | Initial state, everyone in the left bank
locationLeft :: Being -> Bank
locationLeft _ = LeftB

locationLeftR :: R.Relation Being Bank
locationLeftR = R.fromF' locationLeft

-- | Initial state, everyone in the right bank
locationRight :: Being -> Bank
locationRight _ = RightB

locationRightR :: R.Relation Being Bank
locationRightR = R.fromF' locationRight

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
    farmer = R.fromF' (const Farmer)

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
main :: IO ()
main = do
    putStrLn "Monty Hall Problem solution:"
    prettyPrint (secondChoice . firstChoice)
    putStrLn "\n Sum of dices probability:"
    prettyPrint (sumSSM `comp` khatri die die)
    putStrLn "\n Conditional dice throw:"
    prettyPrint conditionalThrows
    putStrLn "\n Checking that the last result is indeed a distribution: "
    prettyPrint (bang . sumSSM . khatri die die)
    putStrLn "\n Probability of grass being wet:"
    prettyPrint (grass_wet . state)
    putStrLn "\n Probability of rain:"
    prettyPrint (rainning . state)
    putStrLn "\n Is the arbitrary state a valid state? (Alcuin Puzzle)"
    print (inv bankStateR)
