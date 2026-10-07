{- |
Module     : LAoP.Matrix.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The matrix type of laop and every operation on it. A matrix is built from
blocks: a single element ('One'), two matrices side by side ('Join', the junc
of Macedo and Oliveira 2013), or two matrices one above the other ('Fork', the
split). Its dimensions are trees of the kind 'Dim', so blocks of mismatched
sizes cannot be put together. "LAoP.Matrix.Indexed" and "LAoP.Matrix.Nat" wrap
this type with friendlier indices.

Import this module to write new block algorithms, or to work with dimension
trees directly. Its API may change between minor versions.

= How composition is computed

Four laws of the papers specify 'comp', and laop 0.2 ran them as its
definition. 'comp' applies the same laws in the order that does the least work.
It lays the operands out with the exchange law ('rowMajor', 'columnMajor'),
splits the result with the fusion laws until each block is a single element
('rowsWithColumns'), and computes each element as the 'dot' product of a row
and a column. Every element is the same sum, grouped the same way, as with the
laws read as a program, so the results are equal bit for bit. 'parComp'
computes the same product on several cores.

= Rewrite rules #rules#

Optimised builds apply these laws of Macedo and Oliveira (2013) wherever a
program states their left-hand side:

@
comp m iden == m                comp iden m == m
tr (tr m) == m                  tr iden == iden
m .+. zeros == m                zeros .+. m == m
m .*. ones == m                 ones .*. m == m
m .*. zeros == zeros            zeros .*. m == zeros
comp p1 i1 == iden              comp p2 i2 == iden            -- eqs. 11, 12
comp p1 i2 == zeros             comp p2 i1 == zeros           -- eqs. 14, 15
comp p1 (fork a b) == a         comp p2 (fork a b) == b       -- eq. 29
comp (join a b) i1 == a         comp (join a b) i2 == b       -- eq. 28
@

The rules assume exact arithmetic: if a matrix holds @NaN@ or an infinity, an
optimised build can return a different result from an unoptimised one, because
a skipped product by zero would have propagated it.
-}
module LAoP.Matrix.Internal (
  -- | Matrix dimensions are tracked by the 'Dim' kind (@U@ for unit,
  -- @:+:@ for sum). The 'FromNat' type family maps type-level naturals
  -- to balanced 'Dim' trees. An 'SDim' is the runtime witness of a 'Dim';
  -- 'KnownDim' supplies it implicitly and 'generate' builds a matrix from an
  -- index function by walking the two witnesses.

  -- * Dimension kind
  Dim (..),

  -- * Dimension singletons
  SDim (SU),
  sPlus,
  sizeOf,
  KnownDim (..),
  dimVal,
  withKnownDim,
  sDimProd,
  sPow,
  sFromNat,
  eqSDim,

  -- * Matrix GADT
  Matrix (..),

  -- * Type families
  FromNat,
  DimProd,
  Pow,

  -- ** Helpers of 'FromNat'
  FromNatPair,
  FromNatStep,
  PairFst,

  -- * Construction from index functions
  generate,
  generateS,

  -- * Layout
  colShape,
  rowShape,
  relayout,

  -- * Primitives
  one,
  join,
  fork,

  -- * Construction
  fromLists,
  toLists,
  toList,
  col,
  row,
  zeros,
  ones,
  bang,
  constant,
  iden,

  -- * Composition and transposition
  comp,
  parComp,
  parCompWith,
  tr,

  -- ** The composition kernel
  rowsWithColumns,
  rowMajor,
  columnMajor,
  dot,

  -- * Element-wise operations
  (.+.),
  (.-.),
  (.*.),
  zipWithM,
  emap,

  -- * Scalar operations
  (.|),
  (./),

  -- * Biproduct
  (===),
  (|||),
  p1,
  p2,
  i1,
  i2,

  -- * Bifunctors
  (-|-),
  (><),

  -- * Pairing
  fstM,
  sndM,
  kr,

  -- * Abide laws
  abideJF,
  abideFJ,

  -- * Block decomposition
  splitJoin,
  splitFork,

  -- * Selective
  select,
  branch,

  -- * Dimensions
  columns,
  columns',
  rows,
  rows',

  -- * Boolean matrices
  Boolean (..),
  Relation,
  toBool,
  fromBool,
  negateM,
  divR,
  divL,
  divS,

  -- * Pretty printing
  pretty,
  prettyPrint,
) where

import           LAoP.Matrix.Internal.Biproduct
import           LAoP.Matrix.Internal.Boolean
import           LAoP.Matrix.Internal.Composition
import           LAoP.Matrix.Internal.Construction
import           LAoP.Matrix.Internal.Dim
import           LAoP.Matrix.Internal.Elementwise
import           LAoP.Matrix.Internal.Kronecker
import           LAoP.Matrix.Internal.Relational
import           LAoP.Matrix.Internal.Representation
