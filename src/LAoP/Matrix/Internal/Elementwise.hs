{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Elementwise
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Operations applied element by element, and scalar operations.
-}
module LAoP.Matrix.Internal.Elementwise (
  (.+.),
  (.-.),
  (.*.),
  zipWithM,
  emap,
  (.|),
  (./),
) where

import           LAoP.Matrix.Internal.Representation

-- Element-wise operations

infixl 6 .+.

{- | Element-wise addition of matrices. It works block by block, as the other
two abide laws of Macedo and Oliveira (2013, eqs. 31 and 32) state:

@
join a b .+. join c d == join (a .+. c) (b .+. d)
fork a b .+. fork c d == fork (a .+. c) (b .+. d)
@
-}
(.+.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.+.) = zipWithM (+)

infixl 6 .-.

-- | Element-wise subtraction of matrices.
(.-.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.-.) = zipWithM (-)

infixl 7 .*.

-- | Element-wise multiplication of matrices (Hadamard product).
(.*.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.*.) = zipWithM (*)

{- | Zip two matrices with a given binary function. When the two are laid out
differently, 'splitJoin' and 'splitFork' line the blocks up as the recursion
goes, so the cost stays linear in the size.
-}
zipWithM :: forall e f g cols rows. (e -> f -> g) -> Matrix e cols rows -> Matrix f cols rows -> Matrix g cols rows
zipWithM f = go
  where
    go :: Matrix e c r -> Matrix f c r -> Matrix g c r
    go (One a) (One b)       = One (f a b)
    go (Join a b) (Join c d) = Join (go a c) (go b d)
    go (Fork a b) (Fork c d) = Fork (go a c) (go b d)
    go (Join a b) y          = case splitJoin y of (c, d) -> Join (go a c) (go b d)
    go (Fork a b) y          = case splitFork y of (c, d) -> Fork (go a c) (go b d)
{-# INLINE zipWithM #-}

-- | Applies a function to every element.
emap :: forall e f cols rows. (e -> f) -> Matrix e cols rows -> Matrix f cols rows
emap f = go
  where
    go :: Matrix e c r -> Matrix f c r
    go (One a)    = One (f a)
    go (Join a b) = Join (go a) (go b)
    go (Fork a b) = Fork (go a) (go b)
{-# INLINE emap #-}

-- Scalar operations

infixl 7 .|

-- | Scalar multiplication of matrices.
{-# SPECIALISE (.|) :: Double -> Matrix Double cols rows -> Matrix Double cols rows #-}
(.|) :: (Num e) => e -> Matrix e cols rows -> Matrix e cols rows
(.|) s (One a)    = One (s * a)
(.|) s (Join a b) = Join (s .| a) (s .| b)
(.|) s (Fork a b) = Fork (s .| a) (s .| b)

infixl 7 ./

-- | Scalar division of matrices.
(./) :: (Fractional e) => Matrix e cols rows -> e -> Matrix e cols rows
(./) (One a) s    = One (a / s)
(./) (Join a b) s = Join (a ./ s) (b ./ s)
(./) (Fork a b) s = Fork (a ./ s) (b ./ s)
{-# INLINABLE (./) #-}
