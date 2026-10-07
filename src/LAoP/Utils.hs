{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module     : LAoP.Utils
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : deprecated

The module of laop 0.2 that held 'Ranged', 'BoundedList' and 'Category'. They
now live in "LAoP.Index" and "LAoP.Category", and this module re-exports them
with the deprecated aliases, so 0.2 imports keep working.
-}
module LAoP.Utils
  {-# DEPRECATED "Import LAoP.Index and LAoP.Category instead" #-}
  (
  -- * 'Ranged' data type
  Ranged,
  pattern Rng,
  mkRanged,
  mkRangedMaybe,

  -- * Coerce auxiliary functions to help promote 'Int' typed functions to
  -- 'Ranged' typed functions.
  coerceRanged,
  coerceRanged2,
  coerceRanged3,

  -- * Deprecated aliases
  Natural,
  reifyToNatural,
  coerceNat,
  coerceNat2,
  coerceNat3,

  -- * Bounded List data type
  BoundedList (..),

  -- * Category type-class
  Category (..),
)
where

import           LAoP.Category       (Category (..))
import           LAoP.Index.Internal (BoundedList (..), Natural, Ranged,
                                      coerceNat, coerceNat2, coerceNat3,
                                      coerceRanged, coerceRanged2,
                                      coerceRanged3, mkRanged, mkRangedMaybe,
                                      pattern Rng, reifyToNatural)
