{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Boolean
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The Boolean semiring, the elements of relations.
-}
module LAoP.Matrix.Internal.Boolean (
  Boolean (..),
  toBool,
  fromBool,
) where

import           Control.DeepSeq (NFData)

-- Boolean matrices

{- | Elements of relations: the boolean semiring, with @+@ as disjunction and
@*@ as conjunction. Composing t'Boolean' matrices with 'LAoP.Matrix.Internal.comp' is therefore
relational composition.

@-@ is truncated subtraction (@a - b@ is @a && not b@), so @.-.@ is relational
difference. 'negate' and the other ring laws do not hold; 'fromInteger' maps
0 to false and every other integer to true. Values show as @0@ and @1@.
-}
newtype Boolean = Boolean Bool
  deriving (Eq, Ord)
  deriving newtype (NFData)

instance Show Boolean where
  showsPrec _ (Boolean b) = showString (if b then "1" else "0")

instance Num Boolean where
  Boolean a + Boolean b = Boolean (a || b)
  Boolean a * Boolean b = Boolean (a && b)
  Boolean a - Boolean b = Boolean (a && not b)
  abs = id
  signum = id
  fromInteger n = Boolean (n /= 0)

-- | Reads a t'Boolean' as a 'Bool'.
toBool :: Boolean -> Bool
toBool (Boolean b) = b

-- | Lifts a 'Bool' to a t'Boolean'.
fromBool :: Bool -> Boolean
fromBool = Boolean
