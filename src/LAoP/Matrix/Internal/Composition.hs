{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Composition
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrix composition, one clause per law.
-}
module LAoP.Matrix.Internal.Composition (
  comp,
) where

import           LAoP.Matrix.Internal.Elementwise    ((.+.))
import           LAoP.Matrix.Internal.Representation (Matrix (..))
{- | Matrix composition: @comp a b@ is the product @a . b@, the matrix that
applies @b@ and then @a@. Each element of the result is a row of @a@ times a
column of @b@: the sum, over the dimension they share, of the products of
their elements.

Each clause is a law of Macedo and Oliveira (2013): the product of two 1 by 1
matrices, divide and conquer (eq. 35), split fusion (eq. 27) and junc fusion
(eq. 26). Together they cover every pair of blocks that type-checks. When
several clauses match, the first one runs, so the layout of the operands
decides the order in which the laws apply.
-}
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (One a)    (One b)    = One (a * b)
comp (Join a b) (Fork c d) = comp a c .+. comp b d
comp (Fork a b) c          = Fork (comp a c) (comp b c)
comp c          (Join a b) = Join (comp c a) (comp c b)
{-# SPECIALISE comp :: Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}
{-# SPECIALISE comp :: Matrix Int cr rows -> Matrix Int cols cr -> Matrix Int cols rows #-}
