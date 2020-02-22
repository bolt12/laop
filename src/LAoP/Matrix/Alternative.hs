{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Void
import Prelude hiding (id, (.), fst, snd, curry, uncurry)

import qualified Prelude

data Matrix e a b where
    Identity :: Matrix e a a
    Zero     :: Matrix e a b
    Scale    :: e -> Matrix e a b -> Matrix e a b
    Add      :: Matrix e a b -> Matrix e a b -> Matrix e a b
    Join     :: Matrix e a c -> Matrix e b c -> Matrix e (Either a b) c
    Fork     :: Matrix e a b -> Matrix e a c -> Matrix e a (Either b c)

instance Category (Matrix e) where
    id = Identity

    Identity  . x         = x
    x         . Identity  = x
    Zero      . _         = Zero
    _         . Zero      = Zero
    Scale x y . z         = Scale x (y . z)
    x         . Scale y z = Scale y (x . z)
    Add x y   . z         = Add (x . z) (y . z)
    x         . Add y z   = Add (x . y) (x . z)
    Join w x  . Fork y z  = Add (w . y) (x . z)
    Fork x y  . z         = Fork (x . z) (y . z)
    x         . Join y z  = Join (x . y) (x . z)

-- Adapted from https://hackage.haskell.org/package/categories
class Category k => Cartesian k where
    type Product k :: * -> * -> *
    fst   :: Product k a b `k` a
    snd   :: Product k a b `k` b
    (&&&) :: (a `k` b) -> (a `k` c) -> a `k` Product k b c

instance Cartesian (->) where
    type Product (->) = (,)
    fst = Prelude.fst
    snd = Prelude.snd
    (f &&& g) a = (f a, g a)

instance Cartesian (Matrix e) where
    type Product (Matrix e) = Either
    fst   = Join Identity Zero
    snd   = Join Zero Identity
    (&&&) = Fork

class Category k => CoCartesian k where
    type Sum k :: * -> * -> *
    inl   :: a `k` Sum k a b
    inr   :: b `k` Sum k a b
    (|||) :: k a c -> k b c -> Sum k a b `k` c

instance CoCartesian (->) where
    type Sum (->) = Either
    inl = Left
    inr = Right
    (f ||| _) (Left  a) = f a
    (_ ||| g) (Right a) = g a

instance CoCartesian (Matrix e) where
    type Sum (Matrix e) = Either
    inl = Fork Identity Zero
    inr = Fork Zero Identity
    (|||) = Join

empty :: Matrix e Void Void
empty = Identity

one :: e -> Matrix e () ()
one e = Scale e Identity

transpose :: Matrix e a b -> Matrix e b a
transpose m = case m of
    Zero      -> Zero
    Identity  -> Identity
    Scale x y -> Scale x (transpose y)
    Add x y   -> Add (transpose x) (transpose y)
    Join x y  -> Fork (transpose x) (transpose y)
    Fork x y  -> Join (transpose x) (transpose y)

select :: Matrix e a (Either b c) -> Matrix e b c -> Matrix e a c
select x y = Join y id . x
