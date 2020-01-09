{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module     : Relation.Internal
-- Copyright  : (c) Armando Santos 2019-2020
-- Maintainer : armandoifsantos@gmail.com
-- Stability  : experimental
--
-- The AoP discipline generalises functions to relations which are 
-- Boolean matrices.
--
-- This module offers many of the combinators of the Algebra of
-- Programming discipline. It is still under construction and very
-- experimental.
--
-- This is an Internal module and it is no supposed to be imported.
--
-----------------------------------------------------------------------------

module Relation.Internal
        ( 
        )
    where

import Data.Void
import qualified Matrix.Internal as I
import Utils
import Control.DeepSeq
import qualified Control.Category as C
import Data.Coerce
import GHC.TypeLits

type Boolean = Natural 0 1
newtype Relation a b = R (I.Matrix Boolean (I.Normalize a) (I.Normalize b))
    deriving (Show, Eq, Ord, NFData) via (I.Matrix (Natural 1 1) (I.Normalize a) (I.Normalize b))

deriving instance (Read (I.Matrix Boolean (I.Normalize a) (I.Normalize b))) => Read (Relation a b)

instance C.Category Relation where
    id = undefined
    (.) = comp

instance Num (Relation a b) where
    (R a) + (R b) = R (I.orM a b)

    (R a) - (R b) = R (I.subM a b)

    (R a) * (R b) = R (I.andM a b)

-- Type alias
type Zero = Void
type One  = ()

-- Primitives

empty :: Relation Zero Zero
empty = R I.Empty

one :: Boolean -> Relation One One
one = R . I.One

junc :: (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) 
     => Relation a c -> Relation b c -> Relation (Either a b) c
junc (R a) (R b) = R (I.Junc a b)

split :: (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) 
      => Relation c a -> Relation c b -> Relation c (Either a b)
split (R a) (R b) = R (I.Split a b)

-- Construction

fromLists :: (I.FromLists Boolean (I.Normalize a) (I.Normalize b)) => [[Boolean]] -> Relation a b
fromLists = R . I.fromLists

identity :: (KnownNat (I.Count (I.Normalize a)),I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a
identity = R I.identity

fromF :: 
      ( Bounded a,
        Bounded b,
        Enum a,
        Enum b,
        Eq b,
        KnownNat (I.Count (I.Normalize c)),
        KnownNat (I.Count (I.Normalize d)),
        I.FromLists Boolean (I.Normalize d) (I.Normalize c)
      )
      => (a -> b) -> Relation c d
fromF f = R (I.fromFRel f)

fromF' :: 
      ( Bounded b,
        Bounded a,
        Enum a,
        Enum b,
        Eq b,
        KnownNat (I.Count (I.Normalize a)),
        KnownNat (I.Count (I.Normalize b)),
        I.FromLists Boolean (I.Normalize b) (I.Normalize a)
      )
      => (a -> b) -> Relation a b
fromF' f = R (I.fromFRel' f)

-- | Relational composition
comp :: forall a b c . Relation b c -> Relation a b -> Relation a c
comp (R a) (R b) = R (I.compRel a b)

-- | Relational converse
conv :: Relation a b -> Relation b a
conv (R a) = R (I.tr a)

-- | Relational inclusion (subset or equal)
sse :: Relation a b -> Relation a b -> Bool
sse a b = a <= b

-- | Relational intersection
--
-- @M `inter` N = M * N@
intersection :: Relation a b -> Relation a b -> Relation a b
intersection a b = a * b

-- | Relational union
--
-- @M `union` N = M + N - M * N
union :: Relation a b -> Relation a b -> Relation a b
union a b = a + b - a * b

-- | Relation Kernel
ker :: Relation a b -> Relation a a
ker r = conv r `comp` r

-- | Relation Image
img :: Relation a b -> Relation b b
img r = r `comp` conv r

-- | Properties of relations
coreflexive :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
coreflexive r = r <= identity

simple :: (KnownNat (I.Count (I.Normalize b)), I.FromLists Boolean (I.Normalize b) (I.Normalize b)) => Relation a b -> Bool
simple = coreflexive . img

injective :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a b -> Bool
injective = simple . conv

-- | Relation pretty printing
pretty :: (KnownNat (I.Count (I.Normalize a))) => Relation a b -> String
pretty (R a) = I.pretty a

-- | Relation pretty printing
prettyPrint :: (KnownNat (I.Count (I.Normalize a))) => Relation a b -> IO ()
prettyPrint (R a) = I.prettyPrint a
