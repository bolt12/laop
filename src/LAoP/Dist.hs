-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module     : LAoP.Dist
Copyright  : (c) Armando Santos 2019-2020
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
motivation behind the library, the underlying theory, and implementation details.

This module exports a type synonym 'Dist'
that represents a stochastic distribution
matrix and several distribution construction
functions.
-}
module LAoP.Dist (
  -- | If the sum of the rows of a column matrix
  -- is equal to 1 then this stochastic matrix
  -- can be seen as a probability distribution.
  --
  --
  -- This module is still experimental but it's
  -- already possible to model probabilistic programming
  -- problems with it. Import 'Matrix.Nat' or 'Matrix.Type'
  -- to access LAoP matrix combinators and then all you have
  -- to do is to define your sample space, either by creating a new data
  -- type or by abstracting it out via 'Natural'.
  --
  -- Write manipulation functions and promote them to matrices via
  -- 'fromF' or 'fromF'' and you're good to go!

  -- * 'Dist' and 'Prob' type synonyms
  Dist (..),
  Prob,

  -- * Constraint type synonyms
  Countable,
  CountableN,
  CountableDimsN,
  FLN,
  Liftable,
  TrivialP,

  -- * Functor instance equivalent functions
  fmapD,

  -- * Applicative equivalent functions
  unitD,
  multD,

  -- * Selective equivalent functions
  selectD,
  branchD,
  ifD,

  -- * Monad equivalent functions
  returnD,
  bindD,

  -- * Distribution construction functions
  choose,
  shape,
  linear,
  uniform,
  negExp,
  normal,

  -- * Converto to list of pairs
  toValues,

  -- * Pretty print distribution
  prettyDist,
  prettyPrintDist,
)
where

import LAoP.Dist.Internal
