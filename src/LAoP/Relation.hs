-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module     : LAoP.Relation
Copyright  : (c) Armando Santos 2019-2020
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The AoP discipline generalises functions to relations which are
Boolean matrices.

This module offers many of the combinators of the Algebra of
Programming discipline. It is still under construction and very
experimental.

This is an Internal module and it is no supposed to be imported.
-}
module LAoP.Relation (
  -- | This definition makes use of the fact that 'Void' is
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
  FromNat,
  Count,
  Normalize,

  -- * Matrix construction and conversion
  FromLists,
  fromLists,
  fromF,
  fromF',
  toRel,
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
  fromF,
  fromF',

  -- ** Relational application
  pointAp,
  pointApBool,

  -- * Matrix printing
  pretty,
  prettyPrint,
)
where

import LAoP.Relation.Internal
