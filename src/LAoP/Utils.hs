{-# LANGUAGE PatternSynonyms #-}

{- |
Module     : LAoP.Utils
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
motivation behind the library, the underlying theory, and implementation details.

This module provides the 'Ranged' data type.
The semantic associated with this data type is that
it's meant to be a restricted 'Int' value.
-}
module LAoP.Utils (
  -- | Utility module that provides the 'Ranged' data type.
  -- The semantic associated with this data type is that
  -- it's meant to be a restricted 'Int' value. For example
  -- the type @Ranged 1 6@ can only be instantiated with @mkRanged \@1 \@6 n@
  -- where @1 <= n <= 6@.

  -- * 'Ranged' data type
  Ranged,
  pattern Rng,
  mkRanged,

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

import           LAoP.Utils.Internal
