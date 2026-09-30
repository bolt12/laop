# laop - Linear Algebra of Programming library

[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![GitHub CI](https://github.com/bolt12/laop/workflows/CI/badge.svg)](https://github.com/bolt12/laop/actions)
[![Hackage](https://img.shields.io/hackage/v/laop.svg?logo=haskell)](https://hackage.haskell.org/package/laop)

The Linear Algebra of Programming (LAoP) extends the Algebra of Programming
from relations, which are Boolean matrices, to matrices over any semiring, all
typed as arrows. Functions are the matrices with a single 1 in each column,
relations the Boolean ones, and probabilistic functions those whose columns are
distributions.

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See [my Msc Thesis](https://github.com/bolt12/master-thesis) for the
motivation behind the library, the underlying theory, and implementation details.

This library offers many of the combinators mentioned in the work of
[Macedo (2012)](https://repositorium.sdum.uminho.pt/handle/1822/22894), [Oliveira (2012)](https://pdfs.semanticscholar.org/ccf5/27fa9179081223bffe8067edd81948644fc0.pdf)
and [Macedo and Oliveira (2013)](https://arxiv.org/abs/1312.4818).

A Functional Pearl has been written and can be regarded as the [reference document](https://github.com/bolt12/tymfgg-pearl) ([ACM link](https://dl.acm.org/doi/abs/10.1145/3406088.3409019)) for this library.

## Features

The library has three matrix modules:

- `LAoP.Matrix.Indexed`: matrices indexed by your own types through the
  `MatIndex` class. An enumeration with a `Generic` instance needs only an
  empty instance.
- `LAoP.Matrix.Nat`: matrices indexed by type-level natural numbers.
- `LAoP.Relation`: Boolean matrices viewed as relations.

`LAoP.Dist` represents probability distributions as column vectors, and
`LAoP.Matrix.Parallel` (with `.Indexed` and `.Nat` variants) multiplies matrices
in parallel with GHC sparks. [PARALLELISM.md](PARALLELISM.md) has the
measurements.

A matrix is an inductive data type over the closed kind
`Dim = U | Dim :+: Dim`: a single element, two matrices side by side, or two
matrices stacked. Algorithms follow that block structure, which is what lets
them be stated and checked with the equational laws of LAoP.

Dimensions are checked by the type checker, so blocks of mismatched sizes cannot
be combined. A few functions check at run time instead and throw on bad input:
the list-based constructors (`fromLists`, `col`, `row`) and `Ranged`
arithmetic.

## Known issues

- Requires GHC 9.10 or later.
- The `Category` instances carry an object constraint (`MatIndex` or
  `KnownNat`), so the `Category` and `Arrow` classes from `base` cannot be
  used.
- In polymorphic `LAoP.Matrix.Nat` code GHC does not derive `KnownNat (a + b)`
  from `KnownNat a` and `KnownNat b`; the `ghc-typelits-knownnat` plugin does.
- The rewrite rules assume exact arithmetic. With `NaN` or infinities in a
  matrix, an optimised build can return a different result from an
  unoptimised one.
- Programs over infinite types, such as lists or integers, do not fit in a
  matrix without a bounded encoding (`Ranged`, `BoundedList`).

## Notes

This is still a work in progress, any feedback is welcome!

## Example

The example below assumes `default-language: GHC2024`. The test suite compiles
it and checks its results (`test/Examples/Readme.hs`).

```Haskell
module Main (main) where

import           GHC.Generics        (Generic)
import           LAoP.Matrix.Indexed
import qualified LAoP.Relation       as R
import           LAoP.Utils
import           Prelude             hiding (id, (.))

-- Monty Hall Problem
data Outcome = Win | Lose
  deriving (Eq, Show, Generic)

instance MatIndex Outcome

switch :: Outcome -> Outcome
switch Win  = Lose
switch Lose = Win

firstChoice :: Matrix Double () Outcome
firstChoice = col [1 / 3, 2 / 3]

secondChoice :: Matrix Double Outcome Outcome
secondChoice = fromF switch

-- Dice sum

type SS = Ranged 1 6 -- Sample Space

sumSS :: SS -> SS -> Ranged 2 12
sumSS = coerceRanged (+)

sumSSM :: Matrix Double (SS, SS) (Ranged 2 12)
sumSSM = fromF (uncurry sumSS)

die :: Matrix Double () SS
die = col $ map (const (1 / 6)) [minBound .. maxBound :: SS]

-- Sprinkler

data G = Dry | Wet
  deriving (Eq, Show, Generic)

instance MatIndex G

data S = Off | On
  deriving (Eq, Show, Generic)

instance MatIndex S

data R = No | Yes
  deriving (Eq, Show, Generic)

instance MatIndex R

rain :: Matrix Double () R
rain = matrixBuilder gen
  where
    gen (_, No)  = 0.8
    gen (_, Yes) = 0.2

sprinkler :: Matrix Double R S
sprinkler = matrixBuilder gen
  where
    gen (No, Off)  = 0.6
    gen (No, On)   = 0.4
    gen (Yes, Off) = 0.99
    gen (Yes, On)  = 0.01

grass :: Matrix Double (S, R) G
grass = matrixBuilder gen
  where
    gen ((Off, No), Dry)  = 1
    gen ((Off, Yes), Dry) = 0.2
    gen ((On, No), Dry)   = 0.1
    gen ((On, Yes), Dry)  = 0.01
    gen ((Off, No), Wet)  = 0
    gen ((Off, Yes), Wet) = 0.8
    gen ((On, No), Wet)   = 0.9
    gen ((On, Yes), Wet)  = 0.99

tag :: (MatIndex b) => Matrix Double b a -> Matrix Double b (a, b)
tag f = kr f iden

state :: Matrix Double (S, R) G -> Matrix Double R S -> Matrix Double () R -> Matrix Double () (G, (S, R))
state g s r = tag g . tag s . r

grassWet :: Matrix Double () (G, (S, R)) -> Matrix Double One One
grassWet s = row [0, 1] . fstM . s

raining :: Matrix Double (G, (S, R)) One
raining = row [0, 1] . sndM . sndM

-- Alcuin Puzzle

data Being = Farmer | Fox | Goose | Beans
  deriving (Eq, Show, Generic)

instance MatIndex Being

data Bank = LeftB | RightB
  deriving (Eq, Show, Generic)

instance MatIndex Bank

eats :: Being -> Being -> Bool
eats Fox Goose   = True
eats Goose Beans = True
eats _ _         = False

eatsR :: R.Relation Being Being
eatsR = R.toRel eats

cross :: Bank -> Bank
cross LeftB  = RightB
cross RightB = LeftB

crossR :: R.Relation Bank Bank
crossR = R.fromF cross

-- Properties

sameBank :: R.Relation Being Bank -> R.Relation Being Being
sameBank = R.ker

canEat :: R.Relation Being Bank -> R.Relation Being Being
canEat w = sameBank w `R.intersection` eatsR

inv :: R.Relation Being Bank -> Bool
inv w = (w `R.comp` canEat w) `R.sse` (w `R.comp` farmer)
  where
    farmer :: R.Relation Being Being
    farmer = R.fromF (const Farmer)

bankState :: Being -> Bank -> Bool
bankState Farmer LeftB = True
bankState Fox LeftB    = True
bankState Goose RightB = True
bankState Beans RightB = True
bankState _ _          = False

bankStateR :: R.Relation Being Bank
bankStateR = R.toRel bankState

-- Main

main :: IO ()
main = do
  putStrLn "Monty Hall Problem solution:"
  prettyPrint (secondChoice . firstChoice)
  putStrLn "\nSum of dices probability:"
  prettyPrint (sumSSM . kr die die)
  putStrLn "\nChecking that the last result is indeed a distribution:"
  prettyPrint (bang . sumSSM . kr die die)
  putStrLn "\nProbability of grass being wet:"
  prettyPrint (grassWet (state grass sprinkler rain))
  putStrLn "\nProbability of rain:"
  prettyPrint (raining . state grass sprinkler rain)
  putStrLn "\nIs the arbitrary state a valid state? (Alcuin Puzzle)"
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

Checking that the last result is indeed a distribution:
┌                    ┐
│ 0.9999999999999999 │
└                    ┘

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
