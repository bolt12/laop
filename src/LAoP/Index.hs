{-# LANGUAGE PatternSynonyms #-}

{- |
Module     : LAoP.Index
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Two index types for matrices, beyond enumerations, 'Either' and pairs.

* @'Ranged' n m@ is an 'Int' between @n@ and @m@, so @'Ranged' 1 6@ can index
  the faces of a die. Build values with 'mkRanged', numeric literals or
  'toEnum', and read them with the 'Rng' pattern; arithmetic that leaves the
  range throws a runtime error.
* @'BoundedList' a@ is a subset of a finite type @a@, which indexes powersets,
  the codomain of 'LAoP.Relation.pt'.

Their @MatIndex@ instances are in "LAoP.Matrix.Indexed".
-}
module LAoP.Index (
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

  -- * 'BoundedList' data type
  BoundedList (..),
)
where

import           LAoP.Index.Internal
