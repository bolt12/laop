{- |
Module     : LAoP.Guide
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

How the library relates to the Linear Algebra of Programming papers, what laop
0.3 changed, and where to find each part. This module exports nothing.

= The Linear Algebra of Programming

The Linear Algebra of Programming (Macedo and Oliveira 2010, 2013; Oliveira
2012) treats matrices as arrows of a category: a matrix with @n@ columns and
@m@ rows is an arrow from @n@ to @m@, and composition is the matrix product.
Every matrix is built from blocks: a single element, two matrices side by side
(the junc @[A|B]@), or two matrices one above the other (the split @[A/B]@).
Laws about blocks, such as fusion, cancellation and the exchange law, turn
matrix algorithms into equational reasoning. Functions are the matrices with a
single 1 in each column, relations are the Boolean matrices, and probabilistic
functions are the matrices whose columns are distributions.

laop is that inductive matrix as a Haskell type. "LAoP.Matrix.Internal" has
three constructors, one for each way to build a block:

@
data Matrix e cols rows where
  One  :: e -> Matrix e U U
  Join :: Matrix e left rows -> Matrix e right  rows -> Matrix e (left :+: right) rows
  Fork :: Matrix e cols top  -> Matrix e cols bottom -> Matrix e cols (top :+: bottom)
@

The dimensions are type-level trees of the kind 'LAoP.Matrix.Internal.Dim'
(@U@ has size 1, @a :+: b@ the size of @a@ plus that of @b@), so blocks of
mismatched sizes cannot be put together: a matrix that type-checks is
well-formed, and the functions on it are total. Algorithms follow the block
structure, which is what lets the laws of the papers state them, test them, and
rewrite some of them away at compile time.

= From the papers to the library

* A matrix @A : n -> m@ is @'LAoP.Matrix.Indexed.Matrix' e n m@: the column
  type comes first, as the source of the arrow.
* Composition @A . B@ is 'LAoP.Matrix.Indexed.comp', or @.@ from
  "LAoP.Category"; the identity is 'LAoP.Matrix.Indexed.iden', or
  'LAoP.Category.id'.
* The junc @[A|B]@ is 'LAoP.Matrix.Indexed.join' (also @|||@), and the split
  @[A/B]@ is 'LAoP.Matrix.Indexed.fork' (also @===@).
* The projections and injections of the biproduct are
  'LAoP.Matrix.Indexed.p1', 'LAoP.Matrix.Indexed.p2',
  'LAoP.Matrix.Indexed.i1' and 'LAoP.Matrix.Indexed.i2'; the direct sum
  @A ⊕ B@ is @-|-@.
* The Khatri-Rao product (pairing) is 'LAoP.Matrix.Indexed.kr', with the
  projections 'LAoP.Matrix.Indexed.fstM' and 'LAoP.Matrix.Indexed.sndM'; the
  Kronecker product @A ⊗ B@ is @><@; the Hadamard product is @.*.@.
* Transposition is 'LAoP.Matrix.Indexed.tr', the converse of a relation
  'LAoP.Relation.conv'.
* The exchange ("abide") law rewrites a matrix with
  'LAoP.Matrix.Indexed.abideJF' and 'LAoP.Matrix.Indexed.abideFJ', and one
  level at a time with 'LAoP.Matrix.Indexed.splitJoin' and
  'LAoP.Matrix.Indexed.splitFork'.

= What laop 0.3 changed

The design is the one above, and the laws hold as they did. What changed is
how the library is typed and computed, and how much it covers:

* Dimensions are the 'LAoP.Matrix.Internal.Dim' kind. In laop 0.2 they were
  @Either@ and @()@ types, with type families to count them.
* Three interfaces wrap the same matrix: "LAoP.Matrix.Indexed" with your own
  index types, "LAoP.Matrix.Nat" with type-level naturals, and
  "LAoP.Relation".
* The product follows the same laws in another order: it lays the operands out
  with the exchange law, applies the fusion laws down to single elements, and
  computes each element as a dot product. It is about four times faster than
  in 0.2 and allocates about 500 times less, with results equal bit for bit.
  <https://github.com/bolt12/laop/blob/master/docs/composition.md docs/composition.md>
  derives it from the laws of 0.2, one step at a time.
* 'LAoP.Matrix.Indexed.parComp' computes the product on several cores, with
  the same results:
  <https://github.com/bolt12/laop/blob/master/docs/parallelism.md docs/parallelism.md>
  has the measurements.
* Relations are matrices over the Boolean semiring, so relational composition
  is matrix composition, and relational division uses the same recursion.
* "LAoP.Dist" builds distributions only through functions that check them.
* Rewrite rules apply the identity, transpose and biproduct laws at compile
  time.

The changelog lists every change and how to upgrade.

= Modules

* "LAoP.Matrix.Indexed": matrices indexed by your own types. Start here.
* "LAoP.Matrix.Nat": matrices indexed by type-level natural numbers.
* "LAoP.Relation": relations, as Boolean matrices, with the combinators of
  the Algebra of Programming.
* "LAoP.Dist": probability distributions, as column vectors.
* "LAoP.Category": the 'LAoP.Category.id' and @.@ that functions and matrices
  share.
* "LAoP.Index": the index types 'LAoP.Index.Ranged' and
  'LAoP.Index.BoundedList'.
* "LAoP.Matrix.Internal": the matrix type, its constructors and the
  composition kernel, for writing new block algorithms. Its API may change
  between minor versions.

"LAoP.Matrix.Type", "LAoP.Utils", "LAoP.Utils.Internal" and
"LAoP.Relation.Internal" are the module names of laop 0.2, kept as deprecated
re-exports.

= Words used in the documentation

[layout] How the 'LAoP.Matrix.Internal.Join' and
  'LAoP.Matrix.Internal.Fork' nodes of a matrix nest. The same matrix has many
  layouts, and the exchange law moves between them without changing it.

[row-major, column-major] The layouts with every
  'LAoP.Matrix.Internal.Fork' above every 'LAoP.Matrix.Internal.Join' (a
  tree of rows), and the other way round (a tree of columns). @fromLists@ builds
  row-major matrices.

[junc, split] The paper's names for two matrices side by side and one above
  the other: 'LAoP.Matrix.Internal.Join' and 'LAoP.Matrix.Internal.Fork'.
  'LAoP.Matrix.Indexed.splitJoin' and 'LAoP.Matrix.Indexed.splitFork' take
  them apart again, in any layout. 'LAoP.Relation.splitR' is different: it is
  the relational pairing, the Khatri-Rao product of relations.

= References

* Hugo Daniel Macedo and José Nuno Oliveira. Matrices as arrows! A biproduct
  approach to typed linear algebra. Mathematics of Program Construction (MPC
  2010), LNCS 6120, 271-287, 2010.
* José Nuno Oliveira. Towards a linear algebra of programming. Formal Aspects
  of Computing 24(4-6), 433-458, 2012.
* Hugo Daniel Macedo. Matrices as arrows: why categories of matrices matter.
  PhD thesis, University of Minho, 2012.
  <https://repositorium.sdum.uminho.pt/handle/1822/22894>
* Hugo Daniel Macedo and José Nuno Oliveira. Typing linear algebra: A
  biproduct-oriented approach. Science of Computer Programming 78(11),
  2160-2191, 2013. <https://arxiv.org/abs/1312.4818>
* Daniel Murta and José Nuno Oliveira. Calculating risk in functional
  programming. 2013. <https://arxiv.org/abs/1311.3687>
* Andrey Mokhov, Georgy Lukyanov, Simon Marlow and Jeremie Dimino. Selective
  applicative functors. Proceedings of the ACM on Programming Languages 3
  (ICFP), 2019.
* Matteo Frigo, Charles E. Leiserson, Harald Prokop and Sridhar Ramachandran.
  Cache-oblivious algorithms. 40th Annual Symposium on Foundations of Computer
  Science, 1999.
* Armando Santos. The master's thesis behind this library.
  <https://github.com/bolt12/master-thesis>
-}
module LAoP.Guide () where
