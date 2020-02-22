{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module     : LAoP.Matrix.Alternative
-- Copyright  : (c) Armando Santos 2019-2020
-- Maintainer : armandoifsantos@gmail.com
-- Stability  : experimental
--
-- The LAoP discipline generalises relations and functions treating them as
-- Boolean matrices and in turn consider these as arrows.
--
-- __LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
-- in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines an alternative matrix data type which has a fully
-- parametric Category instance.
--
-----------------------------------------------------------------------------
module LAoP.Matrix.Alternative where

import Control.Category
import Prelude hiding (id, (.))

data Matrix e a b where
    Identity :: Matrix e a a
    Scale    :: e -> Matrix e a b -> Matrix e a b
    Add      :: Matrix e a b -> Matrix e a b -> Matrix e a b
    Join     :: Matrix e a c -> Matrix e b c -> Matrix e (Either a b) c
    Fork     :: Matrix e a b -> Matrix e a c -> Matrix e a (Either b c)

instance Category (Matrix e) where
    id = Identity

    Identity  . x         = x
    x         . Identity  = x
    Scale x y . z         = Scale x (y . z)
    x         . Scale y z = Scale y (x . z)
    Add x y   . z         = Add (x . z) (y . z)
    x         . Add y z   = Add (x . y) (x . z)
    Join w x  . Fork y z  = Add (w . y) (x . z)
    Fork x y  . z         = Fork (x . z) (y . z)
    x         . Join y z  = Join (x . y) (x . z)
