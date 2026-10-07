{- |
Module     : LAoP.Dist
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Probability distributions as column vectors, and the functions that build and
transform them. 'Dist' is abstract, so every distribution comes from a
function that checks it.
-}
module LAoP.Dist (
  -- | A column vector with non-negative entries that sum to 1 is a
  -- probability distribution. A matrix whose columns are all distributions
  -- (column-stochastic) is a probabilistic function, and composing it with a
  -- distribution gives another distribution.
  --
  --
  -- This module is still experimental, but it can already model
  -- probabilistic programming problems. A sample space is any type with a
  -- 'LAoP.Matrix.Indexed.MatIndex' instance: an enumeration, a @Ranged@
  -- interval, or sums and products of those. Transitions between sample
  -- spaces are matrices from "LAoP.Matrix.Indexed", typically built with
  -- 'LAoP.Matrix.Indexed.fromF' or 'LAoP.Matrix.Indexed.matrixBuilder'.

  -- * The 'Dist' type and 'Prob'
  Dist,
  Prob,
  toMatrix,

  -- * Functor equivalent
  fmapD,

  -- * Applicative equivalent
  unitD,
  multD,

  -- * Selective equivalent
  selectD,

  -- * Monad equivalent
  returnD,
  bindD,

  -- * Distribution construction
  choose,
  shape,
  linear,
  uniform,
  negExp,
  normal,
  fromFreqs,

  -- * Convert to list of pairs
  toValues,

  -- * Pretty print distribution
  prettyDist,
  prettyPrintDist,

  -- * Querying
  (??),
) where

import           LAoP.Dist.Internal
