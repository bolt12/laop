{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Kronecker
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The Khatri-Rao product, its projections, and the Kronecker product.
-}
module LAoP.Matrix.Internal.Kronecker (
  fstM,
  sndM,
  kr,
  (><),
) where

import           LAoP.Matrix.Internal.Construction   (generateS)
import           LAoP.Matrix.Internal.Dim
import           LAoP.Matrix.Internal.Elementwise    ((.|))
import           LAoP.Matrix.Internal.Representation

-- Khatri-Rao product projections

{- | First projection of the Khatri-Rao product, taking the pair @(i, j)@ to
@i@. Macedo and Oliveira (2013, sec. 13) call it @p1@ and define it as
@iden >< bang@.
-}
fstM ::
  forall e m k.
  (Num e, KnownDim m, KnownDim k) =>
  Matrix e (DimProd m k) m
fstM =
  let sm = dimSing @m
      sk = dimSing @k
   in generateS (sDimProd sm sk) sm (\c r -> if div c (sizeOf sk) == r then 1 else 0)

{- | Second projection of the Khatri-Rao product, taking @(i, j)@ to @j@,
defined as @bang >< iden@.
-}
sndM ::
  forall e m k.
  (Num e, KnownDim m, KnownDim k) =>
  Matrix e (DimProd m k) k
sndM =
  let sm = dimSing @m
      sk = dimSing @k
   in generateS (sDimProd sm sk) sk (\c r -> if mod c (sizeOf sk) == r then 1 else 0)

{- | Khatri Rao Matrix product also known as matrix pairing.

The laws below hold for column-stochastic matrices, that is probabilistic
functions (functions included), where Khatri-Rao is a weak product (Murta and
Oliveira 2013, eq. 22). For other matrices each projection scales its factor by
the column sums of the other.

  NOTE: This is not a true categorical product, see for instance:

@
           | fstM . kr a b == a
kr a b ==> |
           | sndM . kr a b == b
@

__Emphasis__ on the implication symbol.
-}
kr ::
  (Num e) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (DimProd a b)
kr (One x) b      = x .| b
kr (Fork a1 a2) b = Fork (kr a1 b) (kr a2 b)
kr (Join a1 a2) b = case splitJoin b of (b1, b2) -> Join (kr a1 b1) (kr a2 b2)
{-# INLINABLE kr #-}

-- Kronecker product

infixl 4 ><

{- | Kronecker product: every element @x@ of @a@ becomes the block @x .| b@.
It is the tensor product of the category of matrices; the categorical product
there is the biproduct (see @-|-@). Macedo and Oliveira (2013, sec. 13) define
it from the Khatri-Rao product,

@
(a >< b) == kr (a . fstM) (b . sndM)
@

and this module computes it block by block, following their Kronecker fusion
laws (eqs. 60 and 61):

@
(join a b >\< c) == join (a >\< c) (b >\< c)
(fork a b >\< c) == fork (a >\< c) (b >\< c)
@

Macedo and Oliveira let products bind tighter than sums, but @><@ is
@infixl 4@: @a >< b .+. c@ parses as @a >< (b .+. c)@, and @a >< b == c@ needs
parentheses.
-}
(><) ::
  (Num e) =>
  Matrix e m p ->
  Matrix e n q ->
  Matrix e (DimProd m n) (DimProd p q)
(><) (One x) b      = x .| b
(><) (Join a1 a2) b = Join (a1 >< b) (a2 >< b)
(><) (Fork a1 a2) b = Fork (a1 >< b) (a2 >< b)
{-# INLINABLE (><) #-}
