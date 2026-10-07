{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Relation
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Relations as Boolean matrices. A @'Relation' a b@ relates values of @a@ to
values of @b@. It is an "LAoP.Matrix.Indexed" matrix over the Boolean
semiring, where @+@ is disjunction and @*@ conjunction, so relational
composition is matrix composition and the matrix laws hold for relations.

This module has the combinators of the Algebra of Programming: converse,
inclusion, the taxonomy of binary relations, division, pairing, coproducts and
currying, each with the laws it satisfies in its documentation.
-}
module LAoP.Relation (
  -- * Relation data type
  Relation (..),
  I.Boolean (..),

  -- * Primitives
  one,
  join,
  (|||),
  fork,
  (===),

  -- * Construction
  fromLists,
  fromF,
  toRel,
  fromRel,
  toLists,
  toList,
  toBool,
  pt,
  belongs,
  relationBuilder,
  zeros,
  ones,
  bang,
  point,

  -- * Relational operations
  conv,
  intersection,
  union,
  complement,
  difference,
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
  fstR,
  sndR,
  (><),

  -- * Relational coproduct
  eitherR,
  i1,
  i2,
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
  predR,
  guard,
  cond,

  -- * Composition and lifting
  iden,
  comp,

  -- * Relational application
  pointAp,
  pointApBool,

  -- * Pretty printing
  pretty,
  prettyPrint,
) where

import           Control.DeepSeq
import           Data.Array           (Array, listArray, (!))
import           GHC.Stack            (HasCallStack)
import           LAoP.Category
import           LAoP.Index           (BoundedList (..))
import qualified LAoP.Matrix.Indexed  as IX
import           LAoP.Matrix.Internal (Boolean)
import qualified LAoP.Matrix.Internal as I
import           Prelude              hiding (id, (.))

{- | Relation data type: a t'Boolean' matrix from "LAoP.Matrix.Indexed". Since
t'Boolean' is a semiring, relational composition is matrix composition.
-}
newtype Relation a b = R (IX.Matrix Boolean a b)
  deriving (Show, Eq, Ord, NFData) via (IX.Matrix Boolean a b)

instance Category Relation where
  type Object Relation a = IX.MatIndex a
  id = iden
  (.) = comp

-- Type alias
type One = ()

-- Primitives

-- | Unit matrix constructor
one :: Boolean -> Relation One One
one = R . IX.one

{- | Boolean Matrix @Join@ constructor, also known as relational coproduct.

See 'eitherR'.
-}
join :: Relation a c -> Relation b c -> Relation (Either a b) c
join (R a) (R b) = R (IX.join a b)

infixl 3 |||

{- | Boolean Matrix @Join@ constructor

See 'eitherR'.
-}
(|||) :: Relation a c -> Relation b c -> Relation (Either a b) c
(|||) = join
{-# INLINE (|||) #-}

{- | Boolean Matrix @Fork@ constructor: the split of two relations into
'Either', which with 'join' forms the biproduct. Fork algebras use "fork" for
pairing, which here is 'splitR'.
-}
fork :: Relation c a -> Relation c b -> Relation c (Either a b)
fork (R a) (R b) = R (IX.fork a b)

infixl 2 ===

-- | Boolean Matrix @Fork@ constructor
(===) :: Relation c a -> Relation c b -> Relation c (Either a b)
(===) = fork
{-# INLINE (===) #-}

-- Construction

{- | Build a matrix out of a list of list of elements. Throws a runtime
error if the dimensions do not match.
-}
fromLists :: (HasCallStack, IX.MatIndex a, IX.MatIndex b) => [[Boolean]] -> Relation a b
fromLists = R . IX.fromLists

{- | Relation builder function. Constructs a relation provided with
a construction function that operates with arbitrary types.
-}
relationBuilder ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  ((a, b) -> Boolean) ->
  Relation a b
relationBuilder = R . IX.matrixBuilder

-- | Lifts functions to 'Relation's with dimensions matching @a@ and @b@.
fromF ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  (a -> b) ->
  Relation a b
fromF = R . IX.fromF

-- | Lifts relation functions to 'Relation'
toRel ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  (a -> b -> Bool) ->
  Relation a b
toRel = R . IX.toRel

{- | Lowers a 'Relation' to a function: @'fromRel' r a b@ tells whether @r@
relates @a@ to @b@, by relational application ('pointApBool'). Every call
multiplies @r@ by a point, in time proportional to the size of @r@, so convert
the relation once with 'toLists' to look up many pairs.
-}
fromRel ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  Relation a b ->
  (a -> b -> Bool)
fromRel r a b = pointApBool a b r

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Relation a b -> [[Boolean]]
toLists (R m) = IX.toLists m

-- | Converts a matrix to a list of elements.
toList :: Relation a b -> [Boolean]
toList (R m) = IX.toList m

-- | Converts a well typed 'Relation' to 'Bool'.
toBool :: Relation One One -> Bool
toBool (R (IX.M (I.One b))) = I.toBool b

{- | Power transpose.

 Maps a relation to a set valued function: @'pt' r a@ lists, in index order,
 every @b@ that @a@ relates to, read off the column @r . 'point' a@. Every
 call computes that product, in time proportional to the size of @r@.
-}
pt ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  Relation a b ->
  (a -> BoundedList b)
pt r a =
  let column = toList (r . point a)
   in L [IX.fromOrd j | (j, v) <- zip [0 ..] column, I.toBool v]

{- | Belongs relation: a set relates to each of its members.

@
'pt' 'belongs' s == s   -- up to order and duplicates
@
-}
belongs ::
  forall a.
  (IX.MatIndex a, Eq a) =>
  Relation (BoundedList a) a
belongs = toRel elemR
  where
    elemR (L l) x = x `elem` l

-- Zeros / Ones / Bang

{- | The zero relation. A relation where no element of type @a@ relates
with elements of type @b@.

  Also known as bottom relation.

  @
  r \`.` bottom == bottom \`.` r == bottom
  bottom ``sse`` R && R ``sse`` T == True
  @
-}
zeros :: (IX.MatIndex a, IX.MatIndex b) => Relation a b
zeros = R IX.zeros

{- | The ones relation. A relation where every element of type @a@ relates
with every element of type @b@.

  Also known as T (Top) Relation or universal Relation.

  @
  bottom ``sse`` R && R ``sse`` T == True
  @
-}
ones :: (IX.MatIndex a, IX.MatIndex b) => Relation a b
ones = R IX.ones

-- | The T (Top) row vector relation.
bang :: (IX.MatIndex a) => Relation a One
bang = ones

-- | Point constant relation
point ::
  forall a.
  (IX.MatIndex a) =>
  a ->
  Relation One a
point = fromF . const

-- Identity

{- | Identity relation

@
'iden' \`.` r == r == r \`.` 'iden'
@
-}
iden :: (IX.MatIndex a) => Relation a a
iden = R IX.iden

-- Composition

{- | Relational composition

@
r \`.` (s \`.` p) = (r \`.` s) \`.` p
@
-}
comp :: Relation b c -> Relation a b -> Relation a c
comp (R a) (R b) = R (IX.comp a b)

-- Division

{- | Relational right division

@'divR' x y@ is the largest relation @z@ which,
pre-composed with @y@, approximates @x@.
-}
divR :: Relation b c -> Relation b a -> Relation a c
divR (R (IX.M x)) (R (IX.M y)) = R (IX.M (I.divR x y))

{- | Relational left division

The dual division operator:

@
'divL' x y == 'conv' ('divR' ('conv' y) ('conv' x))
@
-}
divL :: Relation c b -> Relation a b -> Relation a c
divL (R (IX.M x)) (R (IX.M y)) = R (IX.M (I.divL x y))

{- | Relational symmetric division

@'pointAp' c b ('divS' s r)@ means that @b@ and @c@
are related to exactly the same outputs by @r@ and by @s@.
-}
divS :: Relation c a -> Relation b a -> Relation c b
divS (R (IX.M x)) (R (IX.M y)) = R (IX.M (I.divS x y))

{- | Relational shrinking.

@r ``shrunkBy`` s@ is the largest part of @r@ such that,
if it yields an output for an input @x@, it must be a maximum,
with respect to @s@, among all possible outputs of @x@ by @r@.
-}
shrunkBy :: Relation b a -> Relation a a -> Relation b a
shrunkBy r s = r `intersection` divR s (conv r)

{- | Relational overriding.

@r ``overriddenBy`` s@ yields the relation which contains the
whole of @s@ and that part of @r@ where @s@ is undefined.

@
'zeros' ``overriddenBy`` s == s
r ``overriddenBy`` 'zeros' == r
r ``overriddenBy`` r       == r
@
-}
overriddenBy ::
  (IX.MatIndex b) =>
  Relation a b ->
  Relation a b ->
  Relation a b
overriddenBy r s = s `union` (r `intersection` divR zeros (conv s))

-- Relational application

{- | Relational application.

If @a@ and @b@ are related by 'Relation' @r@
then @'pointAp' a b r == 'one' 1@
-}
pointAp ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  a ->
  b ->
  Relation a b ->
  Relation One One
pointAp a b r = conv (point b) . r . point a

{- | Relational application

The same as 'pointAp' but converts t'Boolean' to 'Bool'
-}
pointApBool ::
  (IX.MatIndex a, IX.MatIndex b) =>
  a ->
  b ->
  Relation a b ->
  Bool
pointApBool a b r = toBool $ conv (point b) . r . point a

-- Converse

{- | Relational converse

Given binary 'Relation' r, writing @'pointAp' a b r@
(read: "@b@ is related to @a@ by @r@") means the same as
@'pointAp' b a ('conv' r)@, where @'conv' r@ is said to be
the converse of @r@.
In terms of grammar, @'conv' r@ corresponds to the passive voice
-}
conv :: Relation a b -> Relation b a
conv (R a) = R (IX.tr a)

-- Set operations

-- | Relational inclusion (subset or equal), element by element.
sse :: Relation a b -> Relation a b -> Bool
sse a b = and (zipWith (<=) (toList a) (toList b))

{- | Relational implication, element by element. It is the relation-valued
counterpart of 'sse': @r \`sse\` s == (implies r s == ones)@.
-}
implies :: Relation a b -> Relation a b -> Relation a b
implies r s = complement r `union` s

-- | Relational bi-implication
iff :: Relation a b -> Relation a b -> Bool
iff r s = r == s

{- | Relational intersection

Lifts pointwise conjunction.

@
(r ``intersection`` s) ``intersection`` t == r ``intersection`` (s ``intersection`` t)
(x ``sse`` (r ``intersection`` s)) == (x ``sse`` r && x ``sse`` s)
@
-}
intersection :: Relation a b -> Relation a b -> Relation a b
intersection (R a) (R b) = R (a IX..*. b)

{- | Relational union

Lifts pointwise disjunction.

@
(r ``union`` s) ``union`` t == r ``union`` (s ``union`` t)
(r ``union`` s) ``sse`` x == (r ``sse`` x && s ``sse`` x)
r \`.` (s ``union`` t) == (r \`.` s) ``union`` (r \`.` t)
(s ``union`` t) \`.` r ==  (s \`.` r) ``union`` (t \`.` r)
@
-}
union :: Relation a b -> Relation a b -> Relation a b
union (R a) (R b) = R (a IX..+. b)

-- | Relational complement (negation)
complement :: Relation a b -> Relation a b
complement (R (IX.M a)) = R (IX.M (I.negateM a))

-- | Relational difference (subtraction)
difference :: Relation a b -> Relation a b -> Relation a b
difference (R a) (R b) = R (a IX..-. b)

-- Kernel and image

{- | Relation Kernel

@
'ker' r == 'conv' r \`.` r
'ker' r == 'img' ('conv' r)
@
-}
ker :: Relation a b -> Relation a a
ker r = conv r . r

{- | Relation Image

@
'img' r == r \`.` 'conv' r
'img' r == 'ker' ('conv' r)
@
-}
img :: Relation a b -> Relation b b
img r = r . conv r

-- Function division

{- | Function division. Special case of 'divS'.

NOTE: This is only valid if @f@ and @g@ are 'function's, i.e. 'simple' and
'entire'.

@'divisionF' f g == 'conv' g \`.` f@
-}
divisionF :: Relation a c -> Relation b c -> Relation a b
divisionF f g = conv g . f

-- Taxonomy of binary relations

-- | A 'Relation' @r@ is 'simple' 'iff' @'coreflexive' ('img' r)@
simple :: (IX.MatIndex b) => Relation a b -> Bool
simple = coreflexive . img

-- | A 'Relation' @r@ is 'injective' 'iff' @'coreflexive' ('ker' r)@
injective :: (IX.MatIndex a) => Relation a b -> Bool
injective = coreflexive . ker

-- | A 'Relation' @r@ is 'entire' 'iff' @'reflexive' ('ker' r)@
entire :: (IX.MatIndex a) => Relation a b -> Bool
entire = reflexive . ker

-- | A 'Relation' @r@ is 'surjective' 'iff' @'reflexive' ('img' r)@
surjective :: (IX.MatIndex b) => Relation a b -> Bool
surjective = reflexive . img

{- | A 'Relation' @r@ is a 'function' 'iff' @'simple' r && 'entire' r@

A 'function' @f@ can be moved from one side of an inclusion to the other by
taking its converse (the shunting rules), where @r@ and @s@ are binary
relations:

@
(f \`.` r) ``sse`` s == r ``sse`` ('conv' f \`.` s)
(r \`.` 'conv' f) ``sse`` s == r ``sse`` (s \`.` f)
@
-}
function :: (IX.MatIndex a, IX.MatIndex b) => Relation a b -> Bool
function r = simple r && entire r

-- | A 'Relation' @r@ is a 'representation' 'iff' @'injective' r && 'entire' r@
representation :: (IX.MatIndex a) => Relation a b -> Bool
representation r = injective r && entire r

-- | A 'Relation' @r@ is an 'abstraction' 'iff' @'surjective' r && 'simple' r@
abstraction :: (IX.MatIndex b) => Relation a b -> Bool
abstraction r = surjective r && simple r

-- | A 'Relation' @r@ is a 'surjection' 'iff' @'function' r && 'abstraction' r@
surjection :: (IX.MatIndex a, IX.MatIndex b) => Relation a b -> Bool
surjection r = function r && abstraction r

-- | A 'Relation' @r@ is a 'injection' 'iff' @'function' r && 'representation' r@
injection :: (IX.MatIndex a, IX.MatIndex b) => Relation a b -> Bool
injection r = function r && representation r

-- | A 'Relation' @r@ is an 'bijection' 'iff' @'injection' r && 'surjection' r@
bijection :: (IX.MatIndex a, IX.MatIndex b) => Relation a b -> Bool
bijection r = injection r && surjection r

-- (Endo-)Relational properties

-- | A 'Relation' @r@ is 'reflexive' 'iff' @'id' ``sse`` r@
reflexive :: (IX.MatIndex a) => Relation a a -> Bool
reflexive r = id `sse` r

-- | A 'Relation' @r@ is 'coreflexive' 'iff' @r ``sse`` 'id'@
coreflexive :: (IX.MatIndex a) => Relation a a -> Bool
coreflexive r = r `sse` id

-- | A 'Relation' @r@ is 'transitive' 'iff' @(r \`.` r) ``sse`` r@
transitive :: Relation a a -> Bool
transitive r = (r . r) `sse` r

-- | A 'Relation' @r@ is 'symmetric' 'iff' @r == 'conv' r@
symmetric :: Relation a a -> Bool
symmetric r = r == conv r

-- | A 'Relation' @r@ is anti-symmetric 'iff' @(r ``intersection`` 'conv' r) ``sse`` 'id'@
antiSymmetric :: (IX.MatIndex a) => Relation a a -> Bool
antiSymmetric r = (r `intersection` conv r) `sse` id

-- | A 'Relation' @r@ is 'irreflexive' 'iff' @(r ``intersection`` 'id') == 'zeros'@
irreflexive :: (IX.MatIndex a) => Relation a a -> Bool
irreflexive r = (r `intersection` id) == zeros

-- | A 'Relation' @r@ is 'connected' 'iff' @(r ``union`` 'conv' r) == 'ones'@
connected :: (IX.MatIndex a) => Relation a a -> Bool
connected r = (r `union` conv r) == ones

-- | A 'Relation' @r@ is a 'preorder' 'iff' @'reflexive' r && 'transitive' r@
preorder :: (IX.MatIndex a) => Relation a a -> Bool
preorder r = reflexive r && transitive r

-- | A 'Relation' @r@ is a partial-order 'iff' @'antiSymmetric' r && 'preorder' r@
partialOrder :: (IX.MatIndex a) => Relation a a -> Bool
partialOrder r = antiSymmetric r && preorder r

-- | A 'Relation' @r@ is a linear-order 'iff' @'connected' r && 'partialOrder' r@
linearOrder :: (IX.MatIndex a) => Relation a a -> Bool
linearOrder r = connected r && partialOrder r

-- | A 'Relation' @r@ is an 'equivalence' 'iff' @'symmetric' r && 'preorder' r@
equivalence :: (IX.MatIndex a) => Relation a a -> Bool
equivalence r = symmetric r && preorder r

{- | A 'Relation' @r@ is a partial equivalence 'iff' @'symmetric' r && 'transitive' r@,
that is, an equivalence on the part of @a@ it is defined on.
-}
partialEquivalence :: Relation a a -> Bool
partialEquivalence r = symmetric r && transitive r

{- | A 'Relation' @r@ is 'difunctional' or regular wherever
@r \`.` 'conv' r \`.` r == r@
-}
difunctional :: Relation a b -> Bool
difunctional r = r . conv r . r == r

-- Relational pairing

{- | Relational pairing.

  NOTE: That this is not a true categorical product, see for instance:

@
               | 'fstR' \`.` 'splitR' a b ``sse`` a
'splitR' a b \<=> |
               | 'sndR' \`.` 'splitR' a b ``sse`` b
@

__Emphasis__ on the 'sse'.

@
'splitR' r s \`.` f == 'splitR' (r \`.` f) (s \`.` f)
(r '><' s) \`.` 'splitR' p q == 'splitR' (r \`.` p) (s \`.` q)
'conv' ('splitR' r s) \`.` 'splitR' x y == ('conv' r \`.` x) ``intersection`` ('conv' s \`.` y)
@

@
'eitherR' ('splitR' r s) ('splitR' t v) == 'splitR' ('eitherR' r t) ('eitherR' s v)
@
-}
splitR ::
  Relation c a ->
  Relation c b ->
  Relation c (a, b)
splitR (R f) (R g) = R (IX.kr f g)

{- | Relational pairing first component projection

@
('fstR' \`.` 'splitR' r s) ``sse`` r
@
-}
fstR ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  Relation (a, b) a
fstR = R IX.fstM

{- | Relational pairing second component projection

@
('sndR' \`.` 'splitR' r s) ``sse`` s
@
-}
sndR ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  Relation (a, b) b
sndR = R IX.sndM

infixl 4 ><

{- | Relational pairing functor

@
(r '><' s) == 'splitR' (r \`.` 'fstR') (s \`.` 'sndR')
(r '><' s) \`.` (p '><' q) == ((r \`.` p) '><' (s \`.` q))
@

@'><'@ is @infixl 4@, like '==', so a comparison with it needs the parentheses.
-}
(><) ::
  Relation a c ->
  Relation b d ->
  Relation (a, b) (c, d)
(><) (R a) (R b) = R (a IX.>< b)

-- Relational coproduct

{- | Relational coproduct.

Its universal property (Macedo and Oliveira 2010, eq. 17): 'eitherR' @a b@ is
the only relation that gives back @a@ and @b@ through the injections.

@
x == 'eitherR' a b  \<=>  x \`.` 'i1' == a && x \`.` 'i2' == b
@

@
'eitherR' r s \`.` 'conv' ('eitherR' t u) == (r \`.` 'conv' t) ``union`` (s \`.` 'conv' u)
@

@
'eitherR' ('splitR' r s) ('splitR' t v) == 'splitR' ('eitherR' r t) ('eitherR' s v)
@
-}
eitherR :: Relation a c -> Relation b c -> Relation (Either a b) c
eitherR = join

{- | Relational coproduct first component injection

@
'img' 'i1' ``union`` 'img' 'i2' == 'id'
'conv' 'i1' \`.` 'i2' == 'zeros'
@
-}
i1 :: (IX.MatIndex a, IX.MatIndex b) => Relation a (Either a b)
i1 = R IX.i1

{- | Relational coproduct second component injection

@
'img' 'i1' ``union`` 'img' 'i2' == 'id'
'conv' 'i1' \`.` 'i2' == 'zeros'
@
-}
i2 :: (IX.MatIndex a, IX.MatIndex b) => Relation b (Either a b)
i2 = R IX.i2

infixl 5 -|-

{- | Relational coproduct functor.

@
r '-|-' s == 'eitherR' ('i1' \`.` r) ('i2' \`.` s)
@
-}
(-|-) ::
  Relation a b ->
  Relation c d ->
  Relation (Either a c) (Either b d)
(-|-) (R a) (R b) = R (a IX.-|- b)

-- Relational "currying"

{- | Relational 'trans'

Every n-ary relation can be expressed as a binary relation through
'trans'/'untrans';
more-over, where each particular attribute is placed (input/output) is irrelevant.

@
'trans' r == 'splitR' r 'sndR' \`.` 'conv' 'fstR'
@

The result is built directly, in time proportional to its size. Computing the
right-hand side as a product takes that time multiplied by the number of
values of @(a, b)@.
-}
trans ::
  forall a b c.
  (IX.MatIndex a, IX.MatIndex b) =>
  Relation (a, b) c ->
  Relation a (c, b)
trans r@(R (IX.M m)) = R (IX.M (I.generateS (IX.dimOf @a) rowsCB element))
  where
    rowsCB = I.sDimProd (I.rowShape m) (IX.dimOf @b)
    related = relates r
    -- trans r relates a to (c, b) when r relates (a, b) to c.
    element a cb = let (c, b) = unpair cb in I.fromBool (related (pair a b) c)
    pair x y = x * IX.cardinality @b + y
    unpair i = i `divMod` IX.cardinality @b

{- | Relational 'untrans'

Every n-ary relation can be expressed as a binary relation through
'trans'/'untrans';
more-over, where each particular attribute is placed (input/output) is irrelevant.

@
'untrans' s == 'fstR' \`.` 'conv' ('splitR' ('conv' s) 'sndR')
@

The result is built directly, in time proportional to its size.
-}
untrans ::
  forall a b c.
  (IX.MatIndex b, IX.MatIndex c) =>
  Relation a (c, b) ->
  Relation (a, b) c
untrans s@(R (IX.M m)) = R (IX.M (I.generateS colsAB (IX.dimOf @c) element))
  where
    colsAB = I.sDimProd (I.colShape m) (IX.dimOf @b)
    related = relates s
    -- untrans s relates (a, b) to c when s relates a to (c, b).
    element ab c = let (a, b) = unpair ab in I.fromBool (related a (pair c b))
    pair x y = x * IX.cardinality @b + y
    unpair i = i `divMod` IX.cardinality @b

-- Whether a relation relates the value at a column position to the value at a
-- row position. It reads the elements once, so apply it to the relation once
-- and query the function it returns.
relates :: Relation a b -> Int -> Int -> Bool
relates r@(R (IX.M m)) =
  let cols = I.sizeOf (I.colShape m)
      xs = map I.toBool (toList r)
      cells = listArray (0, length xs - 1) xs :: Array Int Bool
   in \col row -> cells ! (row * cols + col)

-- Conditionals

{- | Transforms predicate @p@ into a coreflexive relation.

@
'predR' ('fromF' ('const' True)) == 'id'
'predR' ('fromF' ('const' False)) == 'zeros'
@

@
'predR' q \`.` 'predR' p == 'predR' q ``intersection`` 'predR' p
@
-}
predR ::
  forall a.
  (IX.MatIndex a) =>
  Relation a Bool ->
  Relation a a
predR p = id `intersection` divisionF (fromF (const True)) p

{- | Equalizes functions @f@ and @g@.
That is, @'equalizer' f g@ is the largest coreflexive
that restricts @g@ so that @f@ and @g@ yield the same outputs.

@
'equalizer' r r == 'domain' r
'equalizer' f f == 'id'             -- for a function f
'equalizer' ('point' True) ('point' False) == 'zeros'
@
-}
equalizer ::
  (IX.MatIndex a) =>
  Relation a b ->
  Relation a b ->
  Relation a a
equalizer f g = id `intersection` divisionF f g

{- | Relational conditional guard.

@
'guard' p == 'i2' ``overriddenBy`` ('i1' \`.` 'predR' p)
@
-}
guard ::
  forall b.
  (IX.MatIndex b) =>
  Relation b Bool ->
  Relation b (Either b b)
guard p = conv (eitherR (predR p) (predR (complement p)))

-- | Relational McCarthy's conditional.
cond ::
  (IX.MatIndex b) =>
  Relation b Bool ->
  Relation b c ->
  Relation b c ->
  Relation b c
cond p r s = eitherR r s . guard p

-- Domain and range

{- | Relational domain.

For injective relations, 'domain' and 'ker'nel coincide,
since @'ker' r ``sse`` 'id'@ in such situations.
-}
domain :: (IX.MatIndex a) => Relation a b -> Relation a a
domain r = ker r `intersection` id

{- | Relational range.

For functions, 'range' and 'img' (image) coincide,
since @'img' f ``sse`` id@ for any @f@.
-}
range :: (IX.MatIndex b) => Relation a b -> Relation b b
range r = img r `intersection` id

-- Pretty printing

-- | Relation pretty printing
pretty :: Relation a b -> String
pretty (R a) = IX.pretty a

-- | Relation pretty printing
prettyPrint :: Relation a b -> IO ()
prettyPrint (R a) = IX.prettyPrint a
