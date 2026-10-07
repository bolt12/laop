{-# OPTIONS_HADDOCK not-home #-}

{- |
Module     : LAoP.Relation.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : deprecated

The module of laop 0.2 that held the implementation of "LAoP.Relation". It
exported the same names, and now re-exports that module.
-}
module LAoP.Relation.Internal
  {-# DEPRECATED "Import LAoP.Relation instead" #-}
  (module LAoP.Relation)
where

import           LAoP.Relation
