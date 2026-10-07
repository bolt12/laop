# laop - Linear Algebra of Programming library

[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![GitHub CI](https://github.com/bolt12/laop/workflows/CI/badge.svg)](https://github.com/bolt12/laop/actions)
[![Hackage](https://img.shields.io/hackage/v/laop.svg?logo=haskell)](https://hackage.haskell.org/package/laop)

laop is the Linear Algebra of Programming of
[Macedo and Oliveira (2013)](https://arxiv.org/abs/1312.4818) as a Haskell
library. A matrix is a typed arrow from its columns to its rows, built from
blocks: a single element, two matrices side by side, or two matrices one above
the other. Functions are the matrices with a single 1 in each column, relations
are the Boolean matrices, and probabilistic functions are the matrices whose
columns are distributions, so one algebra covers all three.

The functional pearl
[Type Your Matrices for Great Good](https://github.com/bolt12/tymfgg-pearl)
([ACM](https://dl.acm.org/doi/abs/10.1145/3406088.3409019)) describes the design,
and the [master's thesis](https://github.com/bolt12/master-thesis) behind it the
theory.

## Why matrices built from blocks

- Matrices are correct by construction. Dimensions are types, so blocks of
  mismatched sizes cannot be put together: a matrix that type-checks is
  well-formed, and the operations on it are total.
- Algorithms are laws. Each operation is written block by block, after the
  laws of the algebra: fusion, cancellation, the exchange law. The test suite
  checks the laws, and rewrite rules apply some of them at compile time.
- Products are parallel by construction. The blocks of a product do not depend
  on each other, so `parComp` computes them on several cores, with the same
  result bit for bit.
- It is fast enough to use. The product follows the same laws as the paper
  but in the order that does the least work: a 500 by 500 product of `Double`s
  takes 0.34 s on one core and 0.05 s on 16. A library of unboxed arrays such
  as hmatrix is still 85 to 120 times faster on one core; laop trades that for
  the block structure.

## Coming from the paper

The matrices, combinators and laws of the paper are here under their own
names: the junc `[A|B]` is `join`, the split `[A/B]` is `fork`, the
projections and injections are `p1`, `p2`, `i1` and `i2`, and composition is
`comp`, or `.`. The `LAoP.Guide` module has the full table. Since the pearl and
laop 0.2, the design is the same, and these changed:

- Dimensions are trees of the kind `Dim = U | Dim :+: Dim`, where 0.2 used
  `Either` and `()` types.
- Three interfaces wrap the same matrix: your own index types, type-level
  naturals, and relations.
- The product applies the same laws in another order, and is about four times
  faster than in 0.2 with the same results bit for bit.
  [docs/composition.md](https://github.com/bolt12/laop/blob/master/docs/composition.md) derives it from the laws, one step
  at a time.
- `parComp` computes the product on several cores;
  [docs/parallelism.md](https://github.com/bolt12/laop/blob/master/docs/parallelism.md) has the measurements.
- Relations are matrices over the Boolean semiring, and distributions are
  checked when they are built.

The [changelog](https://github.com/bolt12/laop/blob/master/CHANGELOG.md) lists every change and how to upgrade from 0.2.

## Example

```haskell
module Main (main) where

import           GHC.Generics        (Generic)
import           LAoP.Category
import           LAoP.Index
import           LAoP.Matrix.Indexed
import qualified LAoP.Relation       as R
import           Prelude             hiding (id, (.))

-- A function is a matrix, and a distribution is a column.
data Outcome = Win | Lose
  deriving (Eq, Show, Generic)

instance MatIndex Outcome

switch :: Outcome -> Outcome
switch Win  = Lose
switch Lose = Win

firstChoice :: Matrix Double () Outcome
firstChoice = col [1 / 3, 2 / 3]

-- Two independent dice are a pair, made with the Khatri-Rao product.
type Face = Ranged 1 6

die :: Matrix Double () Face
die = col (replicate 6 (1 / 6))

sumOfDice :: Matrix Double () (Ranged 2 12)
sumOfDice = fromF (uncurry (coerceRanged (+))) . kr die die

-- A relation is a Boolean matrix, and composes like one.
data Being = Fox | Goose | Beans
  deriving (Eq, Show, Generic)

instance MatIndex Being

eats :: R.Relation Being Being
eats = R.toRel (\a b -> (a, b) `elem` [(Fox, Goose), (Goose, Beans)])

main :: IO ()
main = do
  prettyPrint (fromF switch . firstChoice)
  prettyPrint sumOfDice
  print (R.pt (eats . eats) Fox)
```

It prints the odds of winning by switching in the Monty Hall problem, the
distribution of the sum of two dice, and what a fox eats through what it eats:

```
┌                    ┐
│ 0.6666666666666666 │
│ 0.3333333333333333 │
└                    ┘
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
L [Beans]
```

Hide Prelude's `id` and `.` to use the ones of `LAoP.Category`, which compose
matrices as they compose functions. A longer example, with a Bayesian network
and the Alcuin puzzle, is in [test/Examples/Readme.hs](https://github.com/bolt12/laop/blob/master/test/Examples/Readme.hs),
which the test suite runs.

## Modules

- `LAoP.Matrix.Indexed`: matrices indexed by your own types. An enumeration
  with a `Generic` instance needs only an empty `MatIndex` instance. Start
  here.
- `LAoP.Matrix.Nat`: matrices indexed by type-level natural numbers.
- `LAoP.Relation`: relations as Boolean matrices, with the combinators of the
  Algebra of Programming.
- `LAoP.Dist`: probability distributions as column vectors.
- `LAoP.Category` and `LAoP.Index`: the shared `id` and `.`, and the index
  types `Ranged` and `BoundedList`.
- `LAoP.Matrix.Internal`: the matrix type and the composition kernel, for new
  block algorithms.
- `LAoP.Guide`: the paper notation, the design decisions, and the references.

## Known issues

- Requires GHC 9.10 or later.
- The `Category` instances carry an object constraint (`MatIndex` or
  `Dimension`), so the `Category` and `Arrow` classes from `base` cannot be
  used.
- In polymorphic `LAoP.Matrix.Nat` code GHC does not derive `Dimension (a + b)`
  from `Dimension a` and `Dimension b`; add it to the context.
- A few functions check at run time and throw on bad input: the list-based
  constructors (`fromLists`, `col`, `row`), `Ranged` construction and
  arithmetic, and the distributions built from probabilities or weights
  (`choose`, `fromFreqs`, `shape`).
- The rewrite rules assume exact arithmetic. With `NaN` or infinities in a
  matrix, an optimised build can return a different result from an
  unoptimised one.
- Programs over infinite types, such as lists or integers, do not fit in a
  matrix without a bounded encoding (`Ranged`, `BoundedList`).
