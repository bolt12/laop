{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
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
    -- isomorphic to 0 and 'One' to 1 and captures matrix
    -- dimensions as stacks of 'Either's.
    --
    -- There exists two type families that make it easier to write
    -- matrix dimensions: 'FromNat' and 'Count'. This approach
    -- leads to a very straightforward implementation 
    -- of LAoP combinators. 

    -- * Relation data type
    Relation (..),
    Boolean,

    -- * Constraint type synonyms
    Countable,
    CountableDims,
    CountableN,
    CountableDimsN,
    FLN,
    Liftable,
    Trivial,
    TrivialP,

    -- * Primitives
    one,
    join,
    (|||),
    fork,
    (===),

    -- * Auxiliary type families
    I.FromNat,
    I.Count,
    I.Normalize,

    -- * Matrix construction and conversion
    I.FromLists,
    fromLists,
    fromF',
    fromF,
    toRel,
    toLists,
    toList,
    toBool,
    pt,
    belongs,
    relationBuilder',
    relationBuilder,
    zeros,
    ones,
    bang,
    point,

    -- * Relational operations
    conv,
    intersection,
    union,
    sse,
    implies,
    iff,
    ker,
    img,

    -- * Taxonomy of binary relations
    injective,
    entire,
    simple,
    surjective,
    representation,
    function,
    abstraction,
    injection,
    surjection,
    bijection,
    domain,
    range,

    -- * Function division
    divisionF,

    -- * Relation division
    divR,
    divL,
    divS,
    shrunkBy,
    overriddenBy,

    -- * Relational pairing
    splitR,

    -- ** Projections
    fstR,
    sndR,
    -- ** Bifunctor
    (><),
    -- * Relational coproduct
    eitherR,
    -- ** Injections
    i1,
    i2,
    -- ** Bifunctor
    (-|-),

    -- * Relational "currying"
    trans,
    untrans,

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
    difunctional,

    -- * Conditionals
    equalizer,

    -- ** McCarthy's Conditional
    predR,
    guard,
    cond,

    -- * Relational composition and lifting
    iden,
    comp,
    fromF',
    fromF,

    -- ** Relational application
    pointAp,
    pointApBool,

    -- * Matrix printing
    pretty,
    prettyPrint
  )
    where

import Data.Void
import qualified LAoP.Matrix.Internal as I
import LAoP.Utils.Internal
import Control.DeepSeq
import Data.Bool
import GHC.TypeLits
import Prelude hiding (id, (.))

-- | Boolean type synonym for working with boolean matrices
type Boolean = Natural 0 1
type Powerset a = List (List a)

-- | Relation data type.
newtype Relation a b = R (I.Matrix Boolean (I.Normalize a) (I.Normalize b))
    deriving (Show, Eq, Ord, NFData) via (I.Matrix (Natural 1 1) (I.Normalize a) (I.Normalize b))

deriving instance (Read (I.Matrix Boolean (I.Normalize a) (I.Normalize b))) => Read (Relation a b)

-- | Constraint type synonyms to keep the type signatures less convoluted
type Countable a        = KnownNat (I.Count a)
type CountableDims a b  = (Countable a, Countable b)
type CountableN a       = KnownNat (I.Count (I.Normalize a))
type CountableDimsN a b = (CountableN a, CountableN b)
type FLN a b            = I.FromLists (I.Normalize a) (I.Normalize b)
type Liftable a b       = (Bounded a, Bounded b, Enum a, Enum b, Eq b, Num Boolean, Ord Boolean)
type Trivial a          = I.Normalize a ~ I.Normalize (I.Normalize a)
type TrivialP a b       = I.Normalize (a, b) ~ I.Normalize (I.Normalize a, I.Normalize b)

-- | It is possible to implement a constrained version of the category type
-- class.
instance Category Relation where
  type Object Relation a = (FLN a a, CountableN a)
  id = iden
  (.) = comp

instance Num (Relation a b) where
    -- | Matrix addition becomes Boolean matrix disjointion
    (R a) + (R b) = R (I.orM a b)

    -- | Matrix subtraction becomes Relational subtraction
    (R a) - (R b) = R (I.subM a b)

    -- | Matrix multiplication becomes Boolean matrix conjointion
    (R a) * (R b) = R (I.andM a b)

    -- | Matrix negation becomes Boolean matrix negation
    negate (R a) = R (I.negateM a) 

-- Type alias
type Zero = Void
type One  = ()

-- Primitives

-- | Unit matrix constructor
one :: Boolean -> Relation One One
one = R . I.One

-- | Boolean Matrix 'Join' constructor, also known as relational coproduct.
--
-- See 'eitherR'.
join :: Relation a c -> Relation b c -> Relation (Either a b) c
join (R a) (R b) = R (I.Join a b)

infixl 3 |||
-- | Boolean Matrix 'Join' constructor
--
-- See 'eitherR'.
(|||) ::
  Relation a c ->
  Relation b c ->
  Relation (Either a b) c
(|||) = join

-- | Boolean Matrix 'Fork' constructor, also known as relational product.
fork :: Relation c a -> Relation c b -> Relation c (Either a b)
fork (R a) (R b) = R (I.Fork a b)

infixl 2 ===
-- | Boolean Matrix 'Fork' constructor
(===) ::
  Relation c a ->
  Relation c b ->
  Relation c (Either a b)
(===) = fork

-- Construction

-- | Build a matrix out of a list of list of elements. Throws a runtime
-- error if the dimensions do not match.
fromLists :: (FLN a b) => [[Boolean]] -> Relation a b
fromLists = R . I.fromLists

-- | Relation builder function. Constructs a relation provided with
-- a construction function that operates with indices.
relationBuilder' ::
  (FLN a b, CountableDimsN a b) =>
  ((Int, Int) -> Boolean) -> Relation a b
relationBuilder' = R . I.matrixBuilder'

-- | Relation builder function. Constructs a relation provided with
-- a construction function that operates with arbitrary types.
relationBuilder ::
  ( FLN a b,
    Enum a,
    Enum b,
    Bounded a,
    Bounded b,
    Eq a,
    CountableDims a b
  ) => ((a, b) -> Boolean) -> Relation a b
relationBuilder = R . I.matrixBuilder

-- | Lifts functions to matrices with arbitrary dimensions.
--
--   NOTE: Be careful to not ask for a matrix bigger than the cardinality of
-- types @a@ or @b@ allows.
fromF' :: 
      ( Liftable a b,
        CountableDimsN c d,
        FLN d c
      )
      => (a -> b) -> Relation c d
fromF' f = R (I.fromFRel' f)

-- | Lifts functions to matrices with dimensions matching @a@ and @b@
-- cardinality's.
fromF ::
      ( Liftable a b,
        CountableDimsN a b,
        FLN b a
      )
      => (a -> b) -> Relation a b
fromF f = R (I.fromFRel f)

-- | Lifts relation functions to 'Relation'
toRel ::
      ( Liftable a b,
        CountableDims a b,
        FLN b a
      )
      => (a -> b -> Bool) -> Relation a b
toRel = R . I.toRel

-- | Lowers a 'Relation' to a function
fromRel ::
        ( Liftable a b,
          Eq a,
          CountableDimsN a b,
          FLN a One,
          FLN b One
        )
        => Relation a b -> (a -> b -> Bool)
fromRel r a b = pointApBool a b r

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Relation a b -> [[Boolean]]
toLists (R m) = I.toLists m

-- | Converts a matrix to a list of elements.
toList :: Relation a b -> [Boolean]
toList (R m) = I.toList m

-- | Converts a well typed 'Relation' to 'Bool'.
toBool :: Relation One One -> Bool
toBool r = case toList r of
  [Nat 0] -> False
  _       -> True

-- | Power transpose.
--
--  Maps a relation to a set valued function.
pt ::
   ( Liftable a b,
     Eq a,
     CountableDimsN a b,
     FLN a One,
     FLN b One
   )
   => Relation a b -> (a -> List b)
pt r a =
  let (L lb) = maxBound
   in L [ b | b <- lb, toBool (pointAp a b r) ]

-- | Belongs relation
belongs ::
        ( Bounded a,
          Enum a,
          Eq a,
          CountableDims (List a) a,
          FLN a (List a)
        )
        => Relation (List a) a
belongs = toRel elemR
  where
    elemR (L l) x = x `elem` l

-- Zeros Matrix

-- | The zero relation. A relation where no element of type @a@ relates
-- with elements of type @b@.
--
--   Also known as ⊥ (Bottom) Relation.
--
--   @
--   r `.` ⊥ == ⊥ `.` r == ⊥
--   ⊥ ``sse`` R && R ``sse`` T == True
--   @
zeros ::
  (FLN a b, CountableDimsN a b) =>
  Relation a b
zeros = relationBuilder' (const (nat 0))

-- Ones Matrix

-- | The ones relation. A relation where every element of type @a@ relates
-- with every element of type @b@.
--
--   Also known as T (Top) Relation or universal Relation.
--
--   @
--   ⊥ ``sse`` R && R ``sse`` T == True
--   @
ones ::
  (FLN a b, CountableDimsN a b) =>
  Relation a b
ones = relationBuilder' (const (nat 1))

-- Bang Matrix

-- | The T (Top) row vector relation.
bang ::
  (FLN a One, CountableN a) =>
  Relation a One
bang = ones

-- | Point constant relation
point ::
      ( Bounded a,
        Enum a,
        Eq a,
        CountableN a,
        FLN a One
      ) => a -> Relation One a
point = fromF . const

-- iden Matrix

-- | iden matrix
--
-- @
-- 'iden' `.` r == r == r `.` 'iden'
-- @
iden ::
  (FLN a a, CountableN a) => Relation a a
iden = relationBuilder' (bool (nat 0) (nat 1) . uncurry (==))

-- | Relational composition
--
-- @
-- r `.` (s `.` p) = (r `.` s) `.` p
-- @
comp :: Relation b c -> Relation a b -> Relation a c
comp (R a) (R b) = R (I.compRel a b)

-- | Relational right division
--
-- @'divR' x y@ is the largest relation @z@ which, 
-- pre-composed with @y@, approximates @x@.
divR :: Relation b c -> Relation b a -> Relation a c
divR (R x) (R y) = R (I.divR x y)

-- | Relational left division
--
-- The dual division operator:
--
-- @
-- 'divL' y x == 'conv' ('divR' ('conv' x) ('conv' y)
-- @
divL :: Relation c b -> Relation a b -> Relation a c
divL (R x) (R y) = R (I.divL x y)

-- | Relational symmetric division
--
-- @'pointAp' c b ('divS' s r)@ means that @b@ and @c@ 
-- are related to exactly the same outputs by @r@ and by @s@.
divS :: Relation c a -> Relation b a -> Relation c b
divS (R x) (R y) = R (I.divS x y)

-- | Relational shrinking.
--
-- @r ``shrunkBy`` s@ is the largest part of @r@ such that,
-- if it yields an output for an input @x@, it must be a maximum,
-- with respect to @s@, among all possible outputs of @x@ by @r@.
shrunkBy :: Relation b a -> Relation a a -> Relation b a
shrunkBy r s = r `intersection` divR s (conv r)

-- | Relational overriding.
--
-- @r ``overriddenBy`` s@ yields the relation which contains the
-- whole of @s@ and that part of @r@ where @s@ is undefined.
--
-- @
-- 'zeros' ``overriddenBy`` s == s
-- r ``overriddenBy`` 'zeros' == r
-- r ``overriddenBy`` r       == r
-- @
overriddenBy :: 
             ( FLN b b,
               CountableN b
             ) => Relation a b -> Relation a b -> Relation a b
overriddenBy r s = s `union` r `intersection` divR zeros (conv s)

-- | Relational application.
--
-- If @a@ and @b@ are related by 'Relation' @r@
-- then @'pointAp' a b r == 'one' ('nat' 1)@
pointAp ::
        ( Liftable a b,
          Eq a,
          CountableDimsN a b,
          FLN a One,
          FLN b One
        ) => a -> b -> Relation a b -> Relation One One
pointAp a b r = conv (point b) . r . point a

-- | Relational application
--
-- The same as 'pointAp' but converts 'Boolean' to 'Bool'
pointApBool ::
        ( Liftable a b,
          Eq a,
          CountableDimsN a b,
          FLN a One,
          FLN b One
        ) => a -> b -> Relation a b -> Bool
pointApBool a b r = toBool $ conv (point b) . r . point a

-- | Relational converse
--
-- Given binary 'Relation' r, writing @'pointAp' a b r@
-- (read: “@b@ is related to @a@ by @r@”) means the same as
-- @'pointAp' b a ('conv' r)@, where @'conv' r@ is said to be 
-- the converse of @r@. 
-- In terms of grammar, @'conv' r@ corresponds to the passive voice
conv :: Relation a b -> Relation b a
conv (R a) = R (I.tr a)

-- | Relational inclusion (subset or equal)
sse :: Relation a b -> Relation a b -> Bool
sse a b = a <= b

-- | Relational implication (the same as @'sse'@)
implies :: Relation a b -> Relation a b -> Relation a b
implies r s = negate r `union` s

-- | Relational bi-implication
iff :: Relation a b -> Relation a b -> Bool
iff r s = r == s

-- | Relational intersection
--
-- Lifts pointwise conjointion.
--
-- @
-- (r ``intersection`` s) ``intersection`` t == r ``intersection`` (s ``intersection`` t)
-- x ``sse`` r ``intersection`` s == x ``intersection`` r && x ``intersection`` s
-- @
intersection :: Relation a b -> Relation a b -> Relation a b
intersection a b = a * b

-- | Relational union
--
-- Lifts pointwise disjointion.
--
-- @
-- (r ``union`` s) ``union`` t == r `'union' (s ``union`` t)
-- r ``union`` s ``sse`` x == r ``sse`` x && s ``sse`` x
-- r `.` (s ``union`` t) == (r `.` s) ``union`` (r `.` t)
-- (s ``union`` t) `.` r ==  (s `.` r) ``union`` (t `.` r)
-- @
union :: Relation a b -> Relation a b -> Relation a b
union a b = a + b

-- | Relation Kernel
--
-- @
-- 'ker' r == 'conv' r `.` r
-- 'ker' r == 'img' ('conv' r)
-- @
ker :: Relation a b -> Relation a a
ker r = conv r . r

-- | Relation Image
--
-- @
-- 'img' r == r `.` conv r
-- 'img' r == 'ker' ('conv' r)
-- @
img :: Relation a b -> Relation b b
img r = r . conv r

-- | Function division. Special case of 'divS'.
--
-- NOTE: _This is only valid_ if @f@ and @g@ are 'function's, i.e. 'simple' and
-- 'entire'.
--
-- @'divisionF' f g == 'conv' g `.` f@
divisionF :: Relation a c -> Relation b c -> Relation a b
divisionF f g = conv g . f

-- Taxonomy of binary relations

-- | A 'Relation' @r@ is 'simple' 'iff' @'coreflexive' ('img' r)@
simple :: (CountableN b, FLN b b) => Relation a b -> Bool
simple = coreflexive . img

-- | A 'Relation' @r@ is 'injective' 'iff' @'coreflexive' ('ker' r)@
injective :: (CountableN a, FLN a a) => Relation a b -> Bool
injective = coreflexive . ker

-- | A 'Relation' @r@ is 'entire' 'iff' @'reflexive' ('ker' r)@
entire :: (CountableN a, FLN a a) => Relation a b -> Bool
entire = reflexive . ker

-- | A 'Relation' @r@ is 'surjective' 'iff' @'reflexive' ('img' r)@
surjective :: (CountableN b, FLN b b) => Relation a b -> Bool
surjective = reflexive . img

-- | A 'Relation' @r@ is a 'function' 'iff' @'simple' r && 'entire' r@
--
-- A 'function' @f@ enjoys the following properties, where @r@ and @s@ are binary
-- relations:
--
-- @
-- f `.` r ``sse`` s == r ``sse`` f `.` s
-- r `.` f ``sse`` s == r ``sse`` s `.` f
-- @
function :: 
         ( CountableDimsN a b,
           FLN a a,
           FLN b b
         ) 
         => Relation a b -> Bool
function r = simple r && entire r

-- | A 'Relation' @r@ is a 'representation' 'iff' @'injective' r && 'entire' r@
representation ::
               ( CountableN a,
                 FLN a a
               )
               => Relation a b -> Bool
representation r = injective r && entire r

-- | A 'Relation' @r@ is an 'abstraction' 'iff' @'surjective' r && 'simple' r@
abstraction ::
            ( CountableN b,
              FLN b b
            )
            => Relation a b -> Bool
abstraction r = surjective r && simple r

-- | A 'Relation' @r@ is a 'surjection' 'iff' @'function' r && 'abstraction' r@
surjection ::
           ( CountableDimsN a b,
             FLN a a,
             FLN b b
           )
           => Relation a b -> Bool
surjection r = function r && abstraction r

-- | A 'Relation' @r@ is a 'injection' 'iff' @'function' r && 'representation' r@
injection ::
           ( CountableDimsN a b,
             FLN a a,
             FLN b b
           )
           => Relation a b -> Bool
injection r = function r && representation r

-- | A 'Relation' @r@ is an 'bijection' 'iff' @'injection' r && 'surjection' r@
bijection :: 
          ( CountableDimsN a b,
            FLN b b,
            FLN a a
          ) => Relation a b -> Bool
bijection r = injection r && surjection r

-- Properties of relations

-- | A 'Relation' @r@ is 'reflexive' 'iff' @'id' ``sse`` r@
reflexive :: (CountableN a, FLN a a) => Relation a a -> Bool
reflexive r = id <= r

-- | A 'Relation' @r@ is 'coreflexive' 'iff' @r ``sse`` 'id'@
coreflexive :: (CountableN a, FLN a a) => Relation a a -> Bool
coreflexive r = r <= id

-- | A 'Relation' @r@ is 'transitive' 'iff' @(r `.` r) ``sse`` r@
transitive :: Relation a a -> Bool
transitive r = (r . r) `sse` r

-- | A 'Relation' @r@ is 'symmetric' 'iff' @r == 'conv' r@
symmetric :: Relation a a -> Bool
symmetric r = r == conv r

-- | A 'Relation' @r@ is anti-symmetric 'iff' @(r ``intersection`` 'conv' r) ``sse`` 'id'@
antiSymmetric :: (CountableN a, FLN a a) => Relation a a -> Bool
antiSymmetric r = (r `intersection` conv r) `sse` id

-- | A 'Relation' @r@ is 'irreflexive' 'iff' @(r ``intersection`` 'id') == 'zeros'@
irreflexive :: (CountableN a, FLN a a) => Relation a a -> Bool
irreflexive r = (r `intersection` id) == zeros

-- | A 'Relation' @r@ is 'connected' 'iff' @(r ``union`` 'conv' r) == 'ones'@
connected :: (CountableN a, FLN a a) => Relation a a -> Bool
connected r = (r `union` conv r) == ones

-- | A 'Relation' @r@ is a 'preorder' 'iff' @'reflexive' r && 'transitive' r@
preorder :: (CountableN a, FLN a a) => Relation a a -> Bool
preorder r = reflexive r && transitive r

-- | A 'Relation' @r@ is a partial-order 'iff' @'antiSymmetric' r && 'preorder' r@
partialOrder :: (CountableN a, FLN a a) => Relation a a -> Bool
partialOrder r = antiSymmetric r && preorder r

-- | A 'Relation' @r@ is a linear-order 'iff' @'connected' r && 'partialOrder' r@
linearOrder :: (CountableN a, FLN a a) => Relation a a -> Bool
linearOrder r = connected r && partialOrder r

-- | A 'Relation' @r@ is an 'equivalence' 'iff' @'symmetric' r && 'preorder' r@
equivalence :: (CountableN a, FLN a a) => Relation a a -> Bool
equivalence r = symmetric r && preorder r

-- | A 'Relation' @r@ is a partial-equivalence 'iff' @'partialOrder' r && 'equivalence' r@
partialEquivalence :: (CountableN a, FLN a a) => Relation a a -> Bool
partialEquivalence r = partialOrder r && equivalence r

-- | A 'Relation' @r@ is 'difunctional' or regular wherever 
-- @r `.` 'conv' r `.` r == r@
difunctional :: Relation a b -> Bool
difunctional r = r . conv r . r == r

-- Relational pairing

-- | Relational pairing.
--
--   NOTE: That this is not a true categorical product, see for instance:
--
-- @
--                | 'fstR' `.` 'splitR' a b ``sse`` a 
-- 'splitR' a b <=>   |
--                | 'sndR' `.` 'splitR' a b ``sse`` b
-- @
--
-- __Emphasis__ on the 'sse'.
--
-- @
-- 'splitR' r s `.` f == 'splitR' (r `.` f) (s `.` f)
-- (R '><' S) `.` 'splitR' p q == 'splitR' (r `.` p) (s `.` q)
-- 'conv' ('splitR' r s) `.` 'splitR' x y == ('conv' r `.` x) ``intersection`` ('conv' s `.` y)
-- @
--
-- @
-- 'eitherR' ('splitR' r s) ('splitR' t v) == 'splitR' ('eitherR' r t) ('eitherR' s v)
-- @
splitR :: 
       ( CountableDimsN a b,
         CountableN (a, b),
         FLN (a, b) a,
         FLN (a, b) b,
         TrivialP a b
       )
       => Relation c a -> Relation c b -> Relation c (a, b)
splitR (R f) (R g) = R (I.kr f g)

-- | Relational pairing first component projection
--
-- @
-- 'fstR' `.` 'splitR' r s ``sse`` r
-- @
fstR ::
   forall a b .
   ( CountableDimsN a b,
     CountableN (a, b),
     FLN (a, b) a,
     TrivialP a b
   )
   => Relation (a, b) a
fstR = R (I.fstM @Boolean @(I.Normalize a) @(I.Normalize b))

-- | Relational pairing second component projection
--
-- @
-- 'sndR' `.` 'splitR' r s ``sse`` s
-- @
sndR ::
   forall a b .
   ( CountableDimsN a b,
     CountableN (a, b),
     FLN (a, b) b,
     TrivialP a b
   )
   => Relation (a, b) b
sndR = R (I.sndM @Boolean @(I.Normalize a) @(I.Normalize b))

-- Relational pairing functor

infixl 4 ><
-- | Relational pairing functor
--
-- @
-- r '><' s == 'splitR' (r `.` fstR) (s `.` sndR)
-- (r '><' s) `.` (p '><' q) == (r `.` p) '><' (s `.` q)
-- @
(><) ::
     ( CountableDimsN a b,
       CountableDimsN c d,
       CountableDimsN (a, c) (b, d),
       FLN (a, c) a,
       FLN (a, c) c,
       FLN (b, d) b,
       FLN (b, d) d,
       TrivialP a c,
       TrivialP b d
     )
     => Relation a b -> Relation c d -> Relation (a, c) (b, d)
(><) (R a) (R b) = R ((I.><) a b)

-- Relational co-products

-- | Relational coproduct.
--
-- @
--                | 'eitherR' a b `.` 'i1' == a
-- 'eitherR' a b <=>  |
--                | 'eitherR' a b `.` 'i2' == b
-- @
--
-- @
-- 'eitherR' r s `.` 'conv' ('eitherR' t u) == (r `.` 'conv' t) ``union`` (s `.` 'conv' u)
-- @
--
-- @
-- 'eitherR' ('splitR' r s) ('splitR' t v) == 'splitR' ('eitherR' r t) ('eitherR' s v)
-- @
eitherR :: Relation a c -> Relation b c -> Relation (Either a b) c
eitherR = join

-- | Relational coproduct first component injection
--
-- @
-- 'img' 'i1' ``union`` 'img' 'i2' == 'id'
-- 'i1' `.` 'i2' = 'zeros'
-- @
i1 :: 
   ( CountableDimsN a b,
     FLN b a,
     FLN a a
   )
   => Relation a (Either a b)
i1 = R I.i1

-- | Relational coproduct second component injection
--
-- @
-- 'img' 'i1' ``union`` 'img' 'i2' == 'id'
-- 'i1' `.` 'i2' = 'zeros'
-- @
i2 :: 
   ( CountableDimsN a b,
     FLN a b,
     FLN b b
   )
   => Relation b (Either a b)
i2 = R I.i2

infixl 5 -|-

-- | Relational coproduct functor.
--
-- @
-- r '-|-' s == 'eitherR' ('i1' `.` r) ('i2' `.` s)
-- @
(-|-) ::
  ( CountableDimsN b d,
    FLN b b,
    FLN d b,
    FLN b d,
    FLN d d
  ) 
  => Relation a b -> Relation c d -> Relation (Either a c) (Either b d)
(-|-) (R a) (R b) = R ((I.-|-) a b)

-- Relational "Currying"

-- | Relational 'trans'
--
-- Every n-ary relation can be expressed as a binary relation through
-- 'trans'/'untrans';
-- more-over, where each particular attribute is placed (input/output) is irrelevant.
trans :: 
      ( CountableDimsN a b,
        CountableN c,
        CountableDimsN (a, b) (c, b),
        FLN (c, b) c,
        FLN (c, b) b,
        FLN (a, b) a,
        FLN (a, b) b,
        Trivial (a, b),
        Trivial (c, b),
        TrivialP a b,
        TrivialP c b
      )
      => Relation (a, b) c -> Relation a (c, b)
trans r = splitR r sndR . conv fstR

-- | Relational 'untrans'
--
-- Every n-ary relation can be expressed as a binary relation through
-- 'trans'/'untrans';
-- more-over, where each particular attribute is placed (input/output) is irrelevant.
untrans ::
        ( CountableDimsN a b,
          CountableN c,
          CountableDimsN (a, b) (c, b),
          FLN (c, b) c,
          FLN (c, b) b,
          FLN (a, b) b,
          FLN (a, b) a,
          Trivial (a, b),
          Trivial (c, b),
          TrivialP a b,
          TrivialP c b
        )
         => Relation a (c, b) -> Relation (a, b) c
untrans s = fstR . conv (splitR (conv s) sndR)

-- | Transforms predicate @p@ into a correflexive relation.
--
-- @
-- 'predR' ('const' True) == 'id'
-- 'predR' ('const' False) == 'zeros'
-- @
--
-- @
-- 'predR' q `.` 'predR' p == 'predR' q ``union`` 'predR' p
-- @
predR :: 
      ( Bounded a,
        Enum a,
        Countable a,
        CountableN a,
        FLN a a,
        FLN Bool a
      )
      => Relation a Bool -> Relation a a
predR p = id `intersection` divisionF (fromF (const True)) p

-- | Equalizes functions @f@ and @g@.
-- That is, @'equalizer' f g@ is the largest coreflexive
-- that restricts @g@ so that @f@ and @g@ yield the same outputs.
--
-- @
-- 'equalizer' r r == 'id'
-- 'equalizer' ('point' True) ('point' False) == 'zeros'
-- @
equalizer ::
          ( CountableN a,
            FLN a a
          )
          => Relation a b -> Relation a b -> Relation a a
equalizer f g = id `intersection` divisionF f g

-- | Relational conditional guard.
--
-- @
-- 'guard' p = 'i2' ``overriddenBy`` 'i1' `.` 'predR' p
-- @
guard ::
     ( Bounded b,
       Enum b,
       CountableN b,
       Countable b,
       FLN b b,
       FLN Bool b
     ) => Relation b Bool -> Relation b (Either b b)
guard p = conv (eitherR (predR p) (predR (negate p)))

-- | Relational McCarthy's conditional.
cond ::
     ( Bounded b,
       Enum b,
       Countable b,
       CountableN b,
       FLN b b,
       FLN Bool b
     )
     => Relation b Bool -> Relation b c -> Relation b c -> Relation b c
cond p r s = eitherR r s . guard p

-- | Relational domain.
--
-- For injective relations, 'domain' and 'ker'nel coincide,
-- since @'ker' r ``sse`` 'id'@ in such situations.
domain :: 
       ( CountableN a,
         FLN a a
       ) => Relation a b -> Relation a a
domain r = ker r `intersection` id

-- | Relational range.
--
-- For functions, 'range' and 'img' (image) coincide,
-- since @'img' f ``sse`` id@ for any @f@.
range :: 
      ( CountableN b,
        FLN b b
      ) => Relation a b -> Relation b b
range r = img r `intersection` id

-- Relation pretty print

-- | Relation pretty printing
pretty :: (CountableDimsN a b) => Relation a b -> String
pretty (R a) = I.pretty a

-- | Relation pretty printing
prettyPrint :: (CountableDimsN a b) => Relation a b -> IO ()
prettyPrint (R a) = I.prettyPrint a
