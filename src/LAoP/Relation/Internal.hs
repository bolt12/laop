{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module     : LAoP.Relation.Internal
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

module LAoP.Relation.Internal
  ( -- | This definition makes use of the fact that 'Void' is
    -- isomorphic to 0 and '()' to 1 and captures matrix
    -- dimensions as stacks of 'Either's.
    --
    -- There exists two type families that make it easier to write
    -- matrix dimensions: 'FromNat' and 'Count'. This approach
    -- leads to a very straightforward implementation 
    -- of LAoP combinators. 

    -- * Relation data type
    Relation (..),
    Boolean,

    -- * Primitives
    empty,
    one,
    junc,
    split,

    -- * Auxiliary type families
    I.FromNat,
    I.Count,
    I.Normalize,

    -- * Matrix construction and conversion
    I.FromLists,
    fromLists,
    toLists,
    toList,
    matrixBuilder,
    zeros,
    ones,
    bang,

    -- * Relational operations
    conv,
    intersection,
    union,
    sse,
    ker,
    img,

    -- * (Endo-)Relational properties
    reflexive,
    coreflexive,
    transitive,
    symmetric,
    antiSymmetric,
    irreflexive,
    connected,
    preorder,
    partialOrder,
    linearOrder,
    equivalence,
    partialEquivalence,

    -- ** McCarthy's Conditional
    -- cond,

    -- * Relational composition and lifting
    identity,
    comp,
    fromF,
    fromF',

    -- * Matrix printing
    pretty,
    prettyPrint
  )
    where

import Data.Void
import qualified LAoP.Matrix.Internal as I
import LAoP.Utils
import Control.DeepSeq
import qualified Control.Category as C
import Data.Coerce
import Data.Bool
import GHC.TypeLits

-- | Boolean type synonym for working with boolean matrices
type Boolean = Natural 0 1

-- | Relation data type.
newtype Relation a b = R (I.Matrix Boolean (I.Normalize a) (I.Normalize b))
    deriving (Show, Eq, Ord, NFData) via (I.Matrix (Natural 1 1) (I.Normalize a) (I.Normalize b))

deriving instance (Read (I.Matrix Boolean (I.Normalize a) (I.Normalize b))) => Read (Relation a b)

-- | It isn't possible to implement the 'id' function so it's
-- implementation is 'undefined'. However 'comp' can be and this partial
-- class implementation exists just to make the code more readable.
--
-- Please use 'identity' instead.
instance C.Category Relation where
    id = undefined
    (.) = comp

instance Num (Relation a b) where
    -- | Matrix addition becomes Boolean matrix disjunction
    (R a) + (R b) = R (I.orM a b)

    -- | Matrix subtraction becomes Relational subtraction
    (R a) - (R b) = R (I.subM a b)

    -- | Matrix multiplication becomes Boolean matrix conjunction
    (R a) * (R b) = R (I.andM a b)

-- Type alias
type Zero = Void
type One  = ()

-- Primitives

-- | Empty matrix constructor
empty :: Relation Zero Zero
empty = R I.Empty

-- | Unit matrix constructor
one :: Boolean -> Relation One One
one = R . I.One

-- | Boolean Matrix 'Junc' constructor
junc :: (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) 
     => Relation a c -> Relation b c -> Relation (Either a b) c
junc (R a) (R b) = R (I.Junc a b)

infixl 3 |||
-- | Boolean Matrix 'Junc' constructor
(|||) ::
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Relation a c ->
  Relation b c ->
  Relation (Either a b) c
(|||) = junc

-- | Boolean Matrix 'Split' constructor
split :: (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) 
      => Relation c a -> Relation c b -> Relation c (Either a b)
split (R a) (R b) = R (I.Split a b)

infixl 2 ===
-- | Boolean Matrix 'Split' constructor
(===) ::
  (I.Normalize (Either a b) ~ Either (I.Normalize a) (I.Normalize b)) =>
  Relation c a ->
  Relation c b ->
  Relation c (Either a b)
(===) = split

-- Construction

-- | Build a matrix out of a list of list of elements. Throws a runtime
-- error if the dimensions do not match.
fromLists :: (I.FromLists Boolean (I.Normalize a) (I.Normalize b)) => [[Boolean]] -> Relation a b
fromLists = R . I.fromLists

-- | Matrix builder function. Constructs a matrix provided with
-- a construction function.
matrixBuilder ::
  (I.FromLists Boolean (I.Normalize a) (I.Normalize b), KnownNat (I.Count (I.Normalize a)), KnownNat (I.Count (I.Normalize b))) =>
  ((Int, Int) -> Boolean) ->
  Relation a b
matrixBuilder = R . I.matrixBuilder

-- | Lifts functions to matrices with arbitrary dimensions.
--
--   NOTE: Be careful to not ask for a matrix bigger than the cardinality of
-- types @a@ or @b@ allows.
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

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
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

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Relation a b -> [[Boolean]]
toLists (R m) = I.toLists m

-- | Converts a matrix to a list of elements.
toList :: Relation a b -> [Boolean]
toList (R m) = I.toList m

-- Zeros Matrix

-- | The zero matrix. A matrix wholly filled with zeros.
zeros ::
  (I.FromLists Boolean (I.Normalize a) (I.Normalize b), KnownNat (I.Count (I.Normalize a)), KnownNat (I.Count (I.Normalize b))) =>
  Relation a b
zeros = matrixBuilder (const (nat 0))

-- Ones Matrix

-- | The ones matrix. A matrix wholly filled with ones.
--
--   Also known as T (Top) matrix.
ones ::
  (I.FromLists Boolean (I.Normalize a) (I.Normalize b), KnownNat (I.Count (I.Normalize a)), KnownNat (I.Count (I.Normalize b))) =>
  Relation a b
ones = matrixBuilder (const (nat 1))

-- Bang Matrix

-- | The T (Top) row vector matrix.
bang ::
  forall e a.
  (I.FromLists Boolean (I.Normalize a) (), KnownNat (I.Count (I.Normalize a))) =>
  Relation a One
bang = ones

-- Identity Matrix

-- | Identity matrix
identity ::
  (I.FromLists Boolean (I.Normalize a) (I.Normalize a), KnownNat (I.Count (I.Normalize a))) =>
  Relation a a
identity = matrixBuilder (bool (nat 0) (nat 1) . uncurry (==))

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

-- | A 'Relation' @r@ is reflexive iff @'identity' `'sse'` r@
reflexive :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
reflexive r = identity <= r

-- | A 'Relation' @r@ is coreflexive iff @r `'sse'` 'identity'@
coreflexive :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
coreflexive r = r <= identity

-- | A 'Relation' @r@ is simple iff @'coreflexive' ('img' r)@
simple :: (KnownNat (I.Count (I.Normalize b)), I.FromLists Boolean (I.Normalize b) (I.Normalize b)) => Relation a b -> Bool
simple = coreflexive . img

-- | A 'Relation' @r@ is injective iff @'coreflexive' ('ker' r)@
injective :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a b -> Bool
injective = coreflexive . ker

-- | A 'Relation' @r@ is entire iff @'reflexive' ('ker' r)@
entire :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a b -> Bool
entire = reflexive . ker

-- | A 'Relation' @r@ is surjective iff @'reflexive' ('img' r)@
surjective :: (KnownNat (I.Count (I.Normalize b)), I.FromLists Boolean (I.Normalize b) (I.Normalize b)) => Relation a b -> Bool
surjective = reflexive . img

-- | A 'Relation' @r@ is transitive iff @(r `'comp'` r) `'sse'` r@
transitive :: Relation a a -> Bool
transitive r = (r `comp` r) `sse` r

-- | A 'Relation' @r@ is symmetric iff @r == r@
symmetric :: Relation a a -> Bool
symmetric r = r == r

-- | A 'Relation' @r@ is anti-symmetric iff @(r `'intersection'` 'conv' r) `'sse'` 'identity'@
antiSymmetric :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
antiSymmetric r = (r `intersection` conv r) `sse` identity

-- | A 'Relation' @r@ is irreflexive iff @(r `'intersection'` identity) == 'zeros'@
irreflexive :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
irreflexive r = (r `intersection` identity) == zeros

-- | A 'Relation' @r@ is connected iff @(r `'union'` 'conv' r) == 'ones'@
connected :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
connected r = (r `union` conv r) == ones

-- | A 'Relation @r@ is a preorder iff @'reflexive' r && 'transitive' r@
preorder :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
preorder r = reflexive r && transitive r

-- | A 'Relation @r@ is a partial order iff @'antiSymmetric' r && 'preorder' r@
partialOrder :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
partialOrder r = antiSymmetric r && preorder r

-- | A 'Relation @r@ is a linear order iff @'connected' r && 'partialOrder' r@
linearOrder :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
linearOrder r = connected r && partialOrder r

-- | A 'Relation @r@ is an equivalence iff @'symmetric' r && 'preorder' r@
equivalence :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
equivalence r = symmetric r && preorder r

-- | A 'Relation @r@ is a partial equivalence iff @'partialOrder' r && 'equivalence' r@
partialEquivalence :: (KnownNat (I.Count (I.Normalize a)), I.FromLists Boolean (I.Normalize a) (I.Normalize a)) => Relation a a -> Bool
partialEquivalence r = partialOrder r && equivalence r

-- Relation pretty print

-- | Relation pretty printing
pretty :: (KnownNat (I.Count (I.Normalize a))) => Relation a b -> String
pretty (R a) = I.pretty a

-- | Relation pretty printing
prettyPrint :: (KnownNat (I.Count (I.Normalize a))) => Relation a b -> IO ()
prettyPrint (R a) = I.prettyPrint a
