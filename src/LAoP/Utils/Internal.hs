{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module     : LAoP.Utils.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : deprecated

The module of laop 0.2 that exposed the raw 'Ranged' constructor. It now lives
in "LAoP.Index.Internal" and the 'Category' class in "LAoP.Category"; this
module re-exports both, so 0.2 imports keep working.
-}
module LAoP.Utils.Internal
  {-# DEPRECATED "Import LAoP.Index.Internal and LAoP.Category instead" #-}
  (
  -- * 'Ranged' data type
  Ranged (UnsafeRanged),
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

  -- * 'BoundedList' data type
  BoundedList (..),

  -- * Category type class
  Category (..),
)
where

import           LAoP.Category       (Category (..))
import           LAoP.Index.Internal (BoundedList (..), Natural, Ranged (..),
                                      coerceNat, coerceNat2, coerceNat3,
                                      coerceRanged, coerceRanged2,
                                      coerceRanged3, mkRanged, mkRangedMaybe,
                                      pattern Rng, reifyToNatural)
