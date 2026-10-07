{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Biproduct
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The standard biproduct: projections, injections, the direct sum, and the
selective operators built from them.
-}
module LAoP.Matrix.Internal.Biproduct (
  p1,
  p2,
  i1,
  i2,
  (-|-),
  select,
  branch,
) where

import           LAoP.Matrix.Internal.Composition    (comp)
import           LAoP.Matrix.Internal.Construction   (iden, tr, zeros)
import           LAoP.Matrix.Internal.Dim
import           LAoP.Matrix.Internal.Elementwise    ((.+.))
import           LAoP.Matrix.Internal.Representation

-- Projections

{- | First projection of the standard biproduct, @[id|0]@ (Macedo and Oliveira
2013, eq. 23). The Khatri-Rao projections are
'LAoP.Matrix.Internal.fstM' and 'LAoP.Matrix.Internal.sndM'.
-}
p1 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e (m :+: n) m
p1 = Join iden zeros

-- | Second biproduct projection, @[0|id]@.
p2 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e (m :+: n) n
p2 = Join zeros iden

-- Injections

-- | First biproduct injection, @[id/0]@, the transpose of 'p1'.
i1 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e m (m :+: n)
i1 = tr p1

-- | Second biproduct injection, @[0/id]@.
i2 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e n (m :+: n)
i2 = tr p2

-- Direct sum

infixl 5 -|-

{- | Direct sum: the block-diagonal matrix with @a@ top left, @b@ bottom right
and zeros elsewhere. It is the bifunctor of the biproduct, so both the
coproduct and the product functor of the category of matrices (Macedo and
Oliveira 2013, eq. 62):

@
a -|- b == join (i1 . a) (i2 . b)
@
-}
(-|-) ::
  (Num e) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (n :+: m) (k :+: j)
(-|-) a b = Join (Fork a (zerosFrom b a)) (Fork (zerosFrom a b) b)

{- | Zero matrix taking its rows from the first argument and its columns from
the second. Needs no dimension constraints.
-}
zerosFrom :: forall e x r c y. (Num e) => Matrix e x r -> Matrix e c y -> Matrix e c r
zerosFrom a b = rowsOf a
  where
    -- Every row is the same zero row, built once.
    zero = zerosRow b
    rowsOf :: Matrix e x' r' -> Matrix e c r'
    rowsOf (Fork a1 a2) = Fork (rowsOf a1) (rowsOf a2)
    rowsOf (Join a1 _)  = rowsOf a1
    rowsOf (One _)      = zero
    zerosRow :: Matrix e c' y' -> Matrix e c' U
    zerosRow (Join b1 b2) = Join (zerosRow b1) (zerosRow b2)
    zerosRow (Fork b1 _)  = zerosRow b1
    zerosRow (One _)      = One 0
{-# INLINABLE zerosFrom #-}

-- Selective

{- | Selective functors 'select' operator equivalent inspired by the
ArrowMonad instance in Selective Applicative Functors (Mokhov et al. 2019).
-}
select ::
  (Num e) =>
  Matrix e cols (a :+: b) ->
  Matrix e a b ->
  Matrix e cols b
select (Fork a b) y   = comp y a .+. b
select (Join m1 m2) y = Join (select m1 y) (select m2 y)
{-# INLINABLE select #-}

{- | Selective functors 'branch' operator: the left component of the input is
sent through the first matrix, the right component through the second. It is
@l@ and @r@ side by side, composed with the input. By divide and conquer
(Macedo and Oliveira 2013, eq. 35) that is @comp l xa .+. comp r xb@ for the
two row blocks @xa@ and @xb@ of the input, the value the 'select' encoding of
the Selective paper gives, without its products by zero blocks.
-}
branch ::
  (Num e) =>
  Matrix e cols (a :+: b) ->
  Matrix e a c ->
  Matrix e b c ->
  Matrix e cols c
branch x l r = comp (Join l r) x
