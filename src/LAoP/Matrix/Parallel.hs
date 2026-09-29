-- |
-- Module      : LAoP.Matrix.Parallel
-- Description : Parallel matrix composition for the inductive GADT representation
--
-- This module provides a parallel version of 'LAoP.Matrix.Internal.comp'
-- (matrix multiplication). Import it qualified so that names mirror the
-- sequential API:
--
-- @
-- import qualified LAoP.Matrix.Parallel as Par
--
-- result = Par.comp a b          -- default depth cutoff (4)
-- tuned  = Par.compWith 6 a b    -- explicit depth
-- @
--
-- The inductive 'Matrix' type decomposes every multiply into two
-- independent sub-computations at each tree level, giving a natural
-- target for spark-based parallelism. At each recursive case the right
-- sub-computation is sparked with @par@ while the left runs on the
-- current thread, then the results combine. A depth cutoff prevents
-- spark creation below a configurable tree level, falling back to
-- sequential 'LAoP.Matrix.Internal.comp' for the leaves.
--
-- A spark evaluates its argument only to weak head normal form. The
-- @StrictData@ pragma on "LAoP.Matrix.Internal" is what makes that enough:
-- every field of @One@, @Join@ and @Fork@ is strict, so a matrix in WHNF is
-- fully evaluated, and sparking a sub-result computes the whole sub-product.
-- Sub-results are bound with @let@ so the spark is created before the strict
-- constructor demands them:
--
-- @
-- let r = compWith (d-1) b c
--     l = compWith (d-1) a c
-- in  r \`par\` l \`pseq\` Fork l r
-- @
--
-- The rewrite rules of "LAoP.Matrix.Internal" match 'LAoP.Matrix.Internal.comp'
-- only; a 'comp' from this module is always computed.
--
-- == Tuning
--
-- 'comp' uses depth 4. At 4 cores depth 3 is enough; at 16 cores deeper
-- cutoffs (up to about 7) still help on large matrices. Keep the default
-- allocation area: @-A64M@ made a 500x500 product about 2.5 times slower at
-- @-N16@.
--
-- Compile with @-threaded -rtsopts@, then run with @+RTS -N@, or
-- @+RTS -N8 -s@ to also print spark statistics.
--
-- <https://github.com/bolt12/laop/blob/master/PARALLELISM.md PARALLELISM.md>
-- (also shipped with the package) has the measured speedups, spark statistics,
-- a depth sweep and a comparison with other Haskell matrix libraries.
module LAoP.Matrix.Parallel (
  -- * Parallel matrix composition
  comp,
  compWith,
) where

import           Control.Parallel     (par, pseq)
import           LAoP.Matrix.Internal (Matrix (..))
import qualified LAoP.Matrix.Internal as I

-- | Parallel matrix composition with a default depth cutoff of 4.
--
-- Drop-in replacement for 'LAoP.Matrix.Internal.comp' when imported
-- qualified. Produces identical results (verified by property tests
-- using approximate equality at 1e-6).
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp = compWith 4
{-# INLINE comp #-}

-- | Parallel matrix composition with an explicit depth cutoff.
--
-- The @depth@ parameter sets how many tree levels create sparks.
-- A depth of 0 or less falls back to sequential
-- 'LAoP.Matrix.Internal.comp'. Depth 3 to 5 covers most use cases;
-- see the module documentation for per-core-count guidance.
compWith :: (Num e) => Int -> Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
compWith d a b | d <= 0 = I.comp a b
compWith _ (One a) (One b) = One (a * b)
compWith d (Join a b) (Fork c d') =
  let l = compWith (d - 1) a c
      r = compWith (d - 1) b d'
  in  r `par` l `pseq` (l I..+. r)
compWith d (Fork a b) c =
  let l = compWith (d - 1) a c
      r = compWith (d - 1) b c
  in  r `par` l `pseq` Fork l r
compWith d c (Join a b) =
  let l = compWith (d - 1) c a
      r = compWith (d - 1) c b
  in  r `par` l `pseq` Join l r
{-# NOINLINE compWith #-}
{-# SPECIALISE [0] compWith :: Int -> Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}
