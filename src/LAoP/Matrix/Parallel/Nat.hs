-- |
-- Module      : LAoP.Matrix.Parallel.Nat
-- Description : Parallel composition for the Nat-indexed matrix API
--
-- Thin wrappers over "LAoP.Matrix.Parallel" that unwrap and rewrap the @M@
-- newtype from "LAoP.Matrix.Nat". Import qualified so names mirror the
-- sequential 'LAoP.Matrix.Nat.comp':
--
-- @
-- import qualified LAoP.Matrix.Parallel.Nat as Par
--
-- result = Par.comp a b
-- tuned  = Par.compWith 6 a b
-- @
--
-- See "LAoP.Matrix.Parallel" for tuning guidance.
module LAoP.Matrix.Parallel.Nat (
  -- * Parallel matrix composition
  comp,
  compWith,
) where

import           Data.Coerce          (coerce)
import           LAoP.Matrix.Nat      (FromNat, Matrix (..))
import qualified LAoP.Matrix.Parallel as P

-- The wrappers are coercions with no arguments of their own, so they inline
-- even when partially applied and the Double specialisation of
-- 'P.compWith' is reached at every call site.

-- | Parallel matrix composition with a default depth cutoff of 4.
comp :: forall e cols cr rows. (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp = coerce (P.comp @e @(FromNat cr) @(FromNat rows) @(FromNat cols))
{-# INLINE comp #-}

-- | Parallel matrix composition with an explicit depth cutoff.
compWith :: forall e cols cr rows. (Num e) => Int -> Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
compWith = coerce (P.compWith @e @(FromNat cr) @(FromNat rows) @(FromNat cols))
{-# INLINE compWith #-}
