# laop - Linear Algebra of Programming library

The LAoP discipline generalises relations and functions treating them as
Boolean matrices and in turn consider these as arrows.

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See [my Msc Thesis](https://github.com/bolt12/master-thesis) for the
motivation behind the library, the underlying theory, and implementation details.

This module offers many of the combinators mentioned in the work of
[Macedo (2012)](https://repositorium.sdum.uminho.pt/handle/1822/22894) and [Oliveira (2012)](https://pdfs.semanticscholar.org/ccf5/27fa9179081223bffe8067edd81948644fc0.pdf). 

## Features

This library offers:

- One that uses normal datatypes (`Void`, `()`, `Either`) for matrix dimensions and two
  type families `FromNat` and `Count` to make it easier to work with these type of matrices.
- Other that uses type level naturals for the dimensions and is just a newtype wrapper
  around the other matrix data type.
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

## Example

```Haskell
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
```

```Shell
Monty Hall Problem solution:
┌                    ┐
│ 0.6666666666666666 │
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
Nat 7  16.666666666666664%
Nat 6  13.88888888888889%
Nat 8  13.88888888888889%
Nat 5  11.11111111111111%
Nat 9  11.11111111111111%
Nat 4  8.333333333333332%
Nat 10  8.333333333333332%
Nat 3  5.555555555555555%
Nat 11  5.555555555555555%
Nat 2  2.7777777777777777%
Nat 12  2.7777777777777777%


 Conditional dice throw:
Nat 9  12.962962962962962%
Nat 12  12.962962962962962%
Nat 10  11.11111111111111%
Nat 11  11.11111111111111%
Nat 8  8.333333333333332%
Nat 13  8.333333333333332%
Nat 6  6.481481481481481%
Nat 15  6.481481481481481%
Nat 7  5.555555555555555%
Nat 14  5.555555555555555%
Nat 3  2.7777777777777777%
Nat 18  2.7777777777777777%
Nat 5  1.8518518518518516%
Nat 16  1.8518518518518516%
Nat 4  0.9259259259259258%
Nat 17  0.9259259259259258%


 Checking that the last result is indeed a distribution: 
┌     ┐
│ 1.0 │
└     ┘

 Probability of grass being wet:
┌         ┐
│ 0.44838 │
└         ┘

 Probability of rain:
┌                     ┐
│ 0.19999999999999998 │
└                     ┘
λ>
```
