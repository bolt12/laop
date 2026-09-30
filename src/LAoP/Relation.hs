{- |
Module     : LAoP.Relation
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The AoP discipline generalises functions to relations which are
Boolean matrices.

This module offers many of the combinators of the Algebra of
Programming discipline. It is still under construction and very
experimental.
-}
module LAoP.Relation (
  -- * Relation data type
  Relation (..),
  Boolean,

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

import           LAoP.Relation.Internal
