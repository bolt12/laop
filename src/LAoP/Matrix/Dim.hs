{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeData             #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Dim
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrix dimensions: the 'Dim' kind, its runtime witnesses ('SDim') and the type
families that build dimensions from naturals, products and powersets.

"LAoP.Matrix.Internal" re-exports everything here except the 'SPlus'
constructor, which only the matrix builders in that module use. This module is
not exposed.
-}
module LAoP.Matrix.Dim (
  -- * Dimension kind
  Dim (..),

  -- * Dimension singletons
  SDim (..),
  sPlus,
  sizeOf,
  KnownDim (..),
  dimVal,
  withKnownDim,
  sDimProd,
  sPow,
  sFromNat,
  unsafeSFromNat,
  eqSDim,

  -- * Type families
  FromNat,
  DimProd,
  Pow,

  -- ** Helpers of 'FromNat'
  FromNatPair,
  FromNatStep,
  PairFst,
) where

import           Data.Proxy         (Proxy (..))
import           Data.Type.Equality (TestEquality (..), (:~:) (..))
import           GHC.Exts           (withDict)
import           GHC.TypeLits
import           Unsafe.Coerce      (unsafeCoerce)

-- Dimension kind

-- | Matrix dimension kind. @U@ is the unit dimension (1), @a :+: b@ is the sum of two dimensions.
type data Dim = U | Dim :+: Dim

infixr 5 :+:

-- Dimension singletons

{- | Runtime witness of a 'Dim'. Every 'SPlus' node caches the size of its
subtree, so 'sizeOf' is O(1). The constructor is not exported: build nodes
with 'sPlus', which computes the cached size.
-}
data SDim (d :: Dim) where
  SU    :: SDim U
  SPlus :: {-# UNPACK #-} !Int -> SDim a -> SDim b -> SDim (a :+: b)

deriving instance Show (SDim d)

-- | Joins two dimension witnesses.
sPlus :: SDim a -> SDim b -> SDim (a :+: b)
sPlus a b = SPlus (sizeOf a + sizeOf b) a b

-- | Number of unit positions in a dimension.
sizeOf :: SDim d -> Int
sizeOf SU            = 1
sizeOf (SPlus n _ _) = n

-- | Supplies the witness of a statically known dimension.
class KnownDim (d :: Dim) where
  dimSing :: SDim d

instance KnownDim U where
  dimSing = SU

instance (KnownDim a, KnownDim b) => KnownDim (a :+: b) where
  dimSing = sPlus (dimSing @a) (dimSing @b)

-- | Value-level size of a statically known dimension.
dimVal :: forall d. (KnownDim d) => Int
dimVal = sizeOf (dimSing @d)

-- | Discharges a 'KnownDim' constraint from an explicit witness.
withKnownDim :: forall d r. SDim d -> ((KnownDim d) => r) -> r
withKnownDim = withDict @(KnownDim d)

-- | Witness of a dimension product, laid out as 'DimProd' lays it out.
sDimProd :: SDim a -> SDim b -> SDim (DimProd a b)
sDimProd SU sb             = sb
sDimProd (SPlus _ a a') sb = sPlus (sDimProd a sb) (sDimProd a' sb)

-- | Witness of a powerset dimension, laid out as 'Pow' lays it out.
sPow :: SDim d -> SDim (Pow d)
sPow SU            = sPlus SU SU
sPow (SPlus _ a b) = sDimProd (sPow a) (sPow b)

{- | Decides whether two witnesses describe the same 'Dim' tree. O(size of the
smaller tree).
-}
eqSDim :: SDim a -> SDim b -> Maybe (a :~: b)
eqSDim SU SU = Just Refl
eqSDim (SPlus n a1 a2) (SPlus m b1 b2)
  | n /= m = Nothing
  | otherwise = do
      Refl <- eqSDim a1 b1
      Refl <- eqSDim a2 b2
      Just Refl
eqSDim _ _ = Nothing

instance TestEquality SDim where
  testEquality = eqSDim

data SomeSDim where
  SomeSDim :: SDim d -> SomeSDim

{- | Witness of @'FromNat' n@ built from the runtime value of @n@. This is
what lets the "LAoP.Matrix.Nat" API ask only for 'KnownNat'.
-}
sFromNat :: forall n. (KnownNat n) => SDim (FromNat n)
sFromNat = unsafeSFromNat @n (fromIntegral (natVal (Proxy @n)))

{- | Witness of @'FromNat' n@ built from an 'Int' that the caller promises is
@n@.

This is the one place the library trusts a value-level computation to agree
with a type family: the tree built here splits exactly where 'FromNat' does
(at @n \`div\` 2@), and the test suite checks the two agree across many sizes.
-}
unsafeSFromNat :: forall n. Int -> SDim (FromNat n)
unsafeSFromNat k0 = case go k0 of
  SomeSDim s -> unsafeCoerce s
  where
    go :: Int -> SomeSDim
    go 1 = SomeSDim SU
    go k
      | k >= 2 =
          let h = k `div` 2
           in case (go h, go (k - h)) of
                (SomeSDim a, SomeSDim b) -> SomeSDim (sPlus a b)
      | otherwise = error ("FromNat: dimension must be >= 1, got " ++ show k)

-- Type families

{- | Maps a type-level natural to a balanced 'Dim' tree: @FromNat n@ is
@FromNat (n \`div\` 2) :+: FromNat (n - n \`div\` 2)@, and @FromNat 1@ is 'U'.

The two halves differ by at most one, so the tree for @n@ is computed together
with the tree for @n + 1@ ('FromNatPair'), both from the pair for
@n \`div\` 2@. Each level then costs one reduction and reuses the subtrees
below it, instead of reducing both halves separately (after an idea of
Li-Yao Xia).
-}
type FromNat :: Nat -> Dim
type family FromNat n where
  FromNat 0 = TypeError (Text "FromNat: dimension must be >= 1")
  FromNat n = PairFst (FromNatPair n)

-- | @FromNatPair n@ is @'(FromNat n, FromNat (n + 1))@.
type FromNatPair :: Nat -> (Dim, Dim)
type family FromNatPair n where
  FromNatPair 1 = '(U, U :+: U)
  FromNatPair n = FromNatStep (Mod n 2) (FromNatPair (Div n 2))

{- | From the pair for @h@ to the pair for @2h@ (first argument 0) or for
@2h + 1@ (first argument 1).
-}
type FromNatStep :: Nat -> (Dim, Dim) -> (Dim, Dim)
type family FromNatStep parity p where
  FromNatStep 0 '(a, b) = '(a :+: a, a :+: b)
  FromNatStep 1 '(a, b) = '(a :+: b, b :+: b)

-- | First component of a type-level pair of dimensions.
type PairFst :: (Dim, Dim) -> Dim
type family PairFst p where
  PairFst '(a, _) = a

{- | Dimension product, distributing over @:+:@ on the left:
@(a :+: a') \* b = a \* b :+: a' \* b@. This is how Macedo and Oliveira (2013,
Theorem 3) type the product of a biproduct, and it puts the index of the first
factor in the major position.
-}
type DimProd :: Dim -> Dim -> Dim
type family DimProd a b where
  DimProd U b = b
  DimProd (a :+: a') b = DimProd a b :+: DimProd a' b

{- | Powerset dimension: a dimension with @n@ positions has a powerset with
@2^n@ positions. Built structurally, so no type-level arithmetic is involved.
-}
type Pow :: Dim -> Dim
type family Pow d where
  Pow U = U :+: U
  Pow (a :+: b) = DimProd (Pow a) (Pow b)
