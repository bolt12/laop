-- |
-- Module      : LAoP.Matrix.Parallel.Indexed
-- Description : Parallel composition for the indexed matrix API
--
-- Thin wrappers over "LAoP.Matrix.Parallel" that unwrap and rewrap
-- the @M@ newtype from "LAoP.Matrix.Indexed". Import qualified so
-- names mirror the sequential 'LAoP.Matrix.Indexed.comp':
--
-- @
-- import qualified LAoP.Matrix.Parallel.Indexed as Par
--
-- result = Par.comp a b
-- tuned  = Par.compWith 6 a b
-- @
--
-- See "LAoP.Matrix.Parallel" for tuning guidance, speedup tables,
-- and background on the strategy choice.
{-# LANGUAGE TypeFamilies #-}

module LAoP.Matrix.Parallel.Indexed (
  -- * Parallel matrix composition
  comp,
  compWith,
) where

import           Data.Coerce          (coerce)
import           LAoP.Matrix.Indexed  (DimOf, Matrix (..))
import qualified LAoP.Matrix.Parallel as P

-- The wrappers are coercions with no arguments of their own, so they inline
-- even when partially applied and the Double specialisation of
-- 'P.compWith' is reached at every call site.

-- | Parallel matrix composition with a default depth cutoff of 4.
comp :: forall e a b c. (Num e) => Matrix e b c -> Matrix e a b -> Matrix e a c
comp = coerce (P.comp @e @(DimOf b) @(DimOf c) @(DimOf a))
{-# INLINE comp #-}

-- | Parallel matrix composition with an explicit depth cutoff.
compWith :: forall e a b c. (Num e) => Int -> Matrix e b c -> Matrix e a b -> Matrix e a c
compWith = coerce (P.compWith @e @(DimOf b) @(DimOf c) @(DimOf a))
{-# INLINE compWith #-}
