# laop - Linear Algebra of Programming library

The LAoP discipline generalises relations and functions treating them as
Boolean matrices and in turn consider these as arrows.

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See [my Msc Thesis](https://github.com/bolt12/master-thesis) for the
motivation behind the library, the underlying theory, and implementation details.

This module offers many of the combinators mentioned in the work of
[Macedo (2012)](https://repositorium.sdum.uminho.pt/handle/1822/22894) and [Oliveira (2012)](https://pdfs.semanticscholar.org/ccf5/27fa9179081223bffe8067edd81948644fc0.pdf). 

See the package in hackage [here](https://hackage.haskell.org/package/laop-0.1.0.5)

There are a couple of repos with a WIP of benchmarks comparing [matrix composition](https://github.com/bolt12/laop-benchmarks) and a real use case of [data analytics](https://github.com/bolt12/laop-analytics).

A Functional Pearl has been submitted to ICFP and can be regarded as the [reference document](https://github.com/bolt12/tymfgg-functional-pearl) for this library.

## Features

This library offers:

- An internal module representation that uses normal datatypes (`Void`, `()`, `Either`) for matrix dimensions and two
  type families `FromNat` and `Count` to make it easier to work with these type of matrices.
- An interfacing module that uses generalised types for the dimensions and is just a newtype wrapper
  around the internal matrix data type.
- An interfacing module that uses type level naturals for the dimensions and is just a newtype wrapper
  around the internal matrix data type.
- An interfacing module that restricts the matrix contents to being only 0's or 1's (True
  and False). This module is called 'Relation' and offers many of the combinators seen in
  AoP and Relational Calculus/Algebra.
- A very simple Probabilistic Programming module and several functions and data types to
  make it easier to deal with sample space.

Given this, this new matrix formulation compared to other libraries one has much more advantages, such as:

- Has an inductive definition enabling writing matrix manipulation functions in a much more elegant and calculational way;
- Statically typed dimensions;
- Polymorphic data type dimensions;
- Polymorphic matrix content;
- Awesome dimension type inference;
- Fast type natural conversion via `FromNat` type family;
- Matrix `Junc` and `Split`-ing in O(1);
- Matrix composition takes advantage of divide-and-conquer and fusion laws.

Unfortunately, this approach does not solve the issue of type dimensions being in some way constrainted making it impossible to write Arrow instances, for example. Type inference isn't perfect, when it comes to infer the types of matrices which dimensions are computed using type level naturals multiplication, the compiler needs type annotations in order to succeed.

## Notes

This is still a work in progress, any feedback is welcome!

## Example

```Haskell
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
```

```Shell
Monty Hall Problem solution:
┌                    ┐
│ 0.6666666666666666 │
│ 0.3333333333333333 │
└                    ┘

 Sum of dices probability:
┌                       ┐
│ 2.7777777777777776e-2 │
│  5.555555555555555e-2 │
│  8.333333333333333e-2 │
│    0.1111111111111111 │
│    0.1388888888888889 │
│   0.16666666666666666 │
│    0.1388888888888889 │
│    0.1111111111111111 │
│  8.333333333333333e-2 │
│  5.555555555555555e-2 │
│ 2.7777777777777776e-2 │
└                       ┘

 Conditional dice throw:
┌                       ┐
│ 2.7777777777777776e-2 │
│  9.259259259259259e-3 │
│ 1.8518518518518517e-2 │
│  6.481481481481481e-2 │
│  5.555555555555555e-2 │
│  8.333333333333333e-2 │
│   0.12962962962962962 │
│    0.1111111111111111 │
│    0.1111111111111111 │
│   0.12962962962962962 │
│  8.333333333333333e-2 │
│  5.555555555555555e-2 │
│  6.481481481481481e-2 │
│ 1.8518518518518517e-2 │
│  9.259259259259259e-3 │
│ 2.7777777777777776e-2 │
└                       ┘

 Checking that the last result is indeed a distribution: 
┌     ┐
│ 1.0 │
└     ┘

 Probability of grass being wet:
┌                    ┐
│ 0.4483800000000001 │
└                    ┘

 Probability of rain:
┌     ┐
│ 0.2 │
└     ┘

Is the arbitrary state a valid state? (Alcuin Puzzle)
False
```
