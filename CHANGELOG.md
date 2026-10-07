# Changelog

`laop` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.3.0.0

Matrix dimensions have a new representation. Most operations keep their names
and meaning, but code that names a dimension constraint or defines its own
index types needs changes. Requires GHC 9.10 or later and is tested with GHC
9.10.3 and 9.12.3. New dependencies: `array` and `parallel`.

### Changed

- `LAoP.Matrix.Indexed` replaces `LAoP.Matrix.Type`. Index types implement
  `MatIndex` instead of `Enum` and `Bounded`, and an enumeration with a
  `Generic` instance only needs an empty instance:

  ```haskell
  -- 0.2
  data Colour = Red | Green | Blue deriving (Bounded, Enum, Generic)

  -- 0.3
  data Colour = Red | Green | Blue deriving (Generic)
  instance MatIndex Colour
  ```

  `()`, `Bool`, `Either`, pairs, `Ranged` and `BoundedList` have instances.
- `LAoP.Matrix.Type` is deprecated and re-exports `LAoP.Matrix.Indexed`, so
  0.2 imports keep resolving, but index types need `MatIndex` instances.
- `LAoP.Relation.Internal` is deprecated and re-exports `LAoP.Relation`, which
  holds the implementation; the two exported the same names.
- Dimensions are the closed kind `Dim = U | Dim :+: Dim` instead of nested
  `Either` and `()`. `FromNat n` still maps a natural to a balanced tree, but
  an odd `n` splits at `n div 2`; 0.2 split off one element first.
- Functions ask for `MatIndex a` in `LAoP.Matrix.Indexed`, `LAoP.Relation`
  and `LAoP.Dist`, and for `Dimension n`, a `KnownNat` of at least 1, in
  `LAoP.Matrix.Nat`. A dimension of 0 is a type error.
- `LAoP.Matrix.Nat.fromF` and `cond` take functions on zero-based indices
  (`Int -> Int` and `Int -> Bool`) instead of functions on `Enum` types.
- The `LAoP.Matrix.Nat` block operations (`join`, `fork`, `p1`, `p2`, `i1`,
  `i2`, `-|-`, `select`, `branch`, `kr`, `fstM`, `sndM`, `><`) accept any
  sizes. When
  the blocks do not line up with the balanced tree of the result, the result is
  laid out again in O(size).
- `Natural` is renamed `Ranged`, `reifyToNatural` is `mkRanged` and
  `coerceNat*` are `coerceRanged*`. The old names remain as deprecated aliases.
  Match values with the `Rng` pattern and build them with `mkRanged`, a
  numeric literal or `toEnum`; the constructor is no longer exported.
- `mkRanged`, `fromInteger`, `coerceRanged`, `coerceRanged2` and the
  arithmetic operators check the range and report the value that failed, and
  `read` rejects a value outside the range. The checks run on `Integer`, so a
  value too large for `Int` fails instead of wrapping into the range.
  `[x ..]` stops at `maxBound`.
- `Ranged` shows and reads as `Rng n` (0.2: `Nat n`) and has no `Generic`
  instance.
- Relations hold `Boolean`, the semiring where `+` is or and `*` is and,
  instead of `Natural 0 1`, and print as `0` and `1`. `LAoP.Relation` and
  `LAoP.Matrix.Indexed` export it with its constructor, `Boolean :: Bool ->
  Boolean`. `Relation` and `Dist` wrap `LAoP.Matrix.Indexed` matrices, so
  relational composition is matrix composition.
- `Matrix`, `Relation` and `Dist` have no `Num` instance. Use `.+.`, `.-.` and
  `.*.` on matrices, and `union`, `difference`, `intersection` and
  `complement` on relations.
- `show` on `LAoP.Matrix.Indexed` and `LAoP.Matrix.Nat` matrices and on
  relations prints the `fromLists` call that builds the value, and on a `Dist`
  the `fromFreqs` call. 0.2 printed the internal block structure.
- `LAoP.Dist` exports `Dist` without its constructor, so every distribution
  sums to 1. Read the vector with `toMatrix`; `LAoP.Dist.Internal` still has
  the constructor.
- `shape`, `uniform`, `linear`, `negExp` and `normal` take a `NonEmpty` list.
  `shape` throws if its function is negative at a point or zero at all of
  them.
- `==` on `BoundedList` compares the subsets, ignoring order and repetition.
- `LAoP.Matrix.Indexed.fromF`, and `generate` and `fromLists` in
  `LAoP.Matrix.Internal`, take the element type as their first type argument,
  like the other builders.
- `Ord` on matrices compares the elements in row-major order. The 0.2 instance
  was an element-wise partial order that broke the `Ord` laws; use
  `LAoP.Relation.sse` for inclusion.
- `.` from `LAoP.Category` (in 0.2, `LAoP.Utils`) is right-associative, like
  `Prelude..`. Chains of
  products group differently, which can change the last bits of a `Double`
  result.
- `fromLists`, `col` and `row` check both dimensions and report the lengths
  they got.
- `kr`, `><` and `-|-` only need `Num e` and follow the block structure
  instead of multiplying by projection matrices, so they run in time
  proportional to the result. `select` and `branch` compute only the products
  they contain.
- `comp` lays its operands out once and computes every element as a dot
  product, with the same sums in the same order, so results are the same bit
  for bit. On `Double` matrices it is 3.5 to 4.4 times faster (500x500: 1.20 s
  to 0.34 s; 1000x1000: 13.8 s to 3.2 s) and allocates about 500 times less,
  and a left operand laid out by columns (a `tr` or `conv`) no longer makes it
  ten times slower. It is specialised for `Double`, `Int` and `Boolean`.
  `docs/composition.md` derives it from the laws of 0.2, step by step.
- `divR`, and with it `divL` and `divS`, use the recursion of `comp`. On
  200x200 relations `divR` is 8 times faster, and 18 times faster when the
  relations are laid out by columns.
- `fmapD`, `trans` and `untrans` build their result directly, in time
  proportional to it, instead of multiplying by a dense matrix.

### Added

- `parComp` and `parCompWith` in `LAoP.Matrix.Internal`, `LAoP.Matrix.Indexed`,
  `LAoP.Matrix.Nat` and `LAoP.Relation`: `parComp` computes the blocks of the
  result in parallel, with a depth chosen from the number of cores and the size
  of the product, and `parCompWith` takes the depth. The results are those of
  `comp`, bit for bit. See `docs/parallelism.md`.
- `LAoP.Category` holds the `Category` class, and `LAoP.Index` the index types
  `Ranged` and `BoundedList`, with the raw `Ranged` constructor in
  `LAoP.Index.Internal`. `LAoP.Utils` and `LAoP.Utils.Internal` re-export them
  as before, and are deprecated.
- `LAoP.Guide`, a documentation module that relates the library to the papers
  and lists the references.
- `rowsWithColumns`, `rowMajor`, `columnMajor` and `dot` in
  `LAoP.Matrix.Internal`: the recursion behind `comp`, for other algorithms
  that combine every row of one matrix with every column of another.
- Rewrite rules for the identity, transpose, zero, one and biproduct laws,
  such as `comp m iden = m` and `comp p1 (fork a b) = a`. They assume exact
  arithmetic, so with `NaN` or infinities an optimised build can return a
  different result from an unoptimised one. `parComp` is never rewritten.
- A `MatIndex` instance for `BoundedList`, so `pt` and `belongs` no longer
  need `Enum` or `Bounded`.
- `splitJoin` and `splitFork` in `LAoP.Matrix.Internal`, `LAoP.Matrix.Indexed`
  and `LAoP.Matrix.Nat` take a matrix apart into its column or row blocks,
  whatever its layout.
- `LAoP.Dist.fromFreqs`, `(??)` and `toMatrix`, `LAoP.Relation.complement`,
  `difference` and `fromRel`, `LAoP.Matrix.Indexed.cardinality`, `select` and
  `branch`, `LAoP.Matrix.Nat.point` and `branch`, `scalar` and `emap` for
  `LAoP.Matrix.Indexed` and `LAoP.Matrix.Nat`, `LAoP.Index.mkRangedMaybe`, and
  a `Category` instance for `LAoP.Matrix.Nat` matrices.

### Removed

- The constraint synonyms (`Countable*`, `FL*`, `Liftable`, `Trivial*`), the
  `Count` and `Normalize` type families, the `FromLists` class and the `Zero`
  type.
- `fromF'` from every module (use `fromF`), and `fmapM`, `returnM`, `bindM`,
  `columns'` and `rows'` from `LAoP.Matrix.Type`, now `LAoP.Matrix.Indexed`.
  `LAoP.Matrix.Internal` still has `columns'` and `rows'`.
- `LAoP.Dist.branchD` and `ifD`.
- `FromNat` from `LAoP.Relation` and `LAoP.Matrix.Type`; `LAoP.Matrix.Nat`
  exports it.
- From `LAoP.Matrix.Internal`: the `Category` instance, `matrixBuilder`,
  `matrixBuilder'`, `cond`, `compRel`, `fromFRel`, `fromFRel'`, `toRel`, `orM`,
  `andM` and `subM`. The typed modules have them.
- The orphan `Enum (a, b)`, `Enum (Either a b)` and `Bounded (Either a b)`
  instances.
- Function types and constructors with fields as dimensions. Build such index
  types from `Either`, pairs and `Ranged`, or write the `MatIndex` instance by
  hand.

### Fixed

- `overriddenBy r s` computed `(s ∪ r) ∩ (⊥ / s°)` instead of
  `s ∪ (r ∩ (⊥ / s°))`.
- `partialEquivalence` also required reflexivity and antisymmetry, so only the
  identity passed. It is now `symmetric r && transitive r`.
- `choose` accepted any type and failed at run time unless the type had
  exactly two values. The type now has to have two values.
- `choose` throws on a probability outside `[0, 1]`. In 0.2 it returned a
  vector with a negative entry.
- `uniform`, `linear`, `negExp`, `normal` and `shape` put each weight on its
  own outcome, add up repeated outcomes and give a lone outcome probability 1.
  In 0.2 the list had to name every value once and in order, or the call failed
  or the probabilities landed on the wrong outcomes.
- `fromEnum` on a `BoundedList` ignores the order of its elements and
  duplicates. In 0.2 it failed unless the list followed the order of the type.

## 0.2.0.0

- Compiles with GHC 9.2.8
- Update nix infrastructure
- Cleans up death code and warnings
- Changed `nat` to `reifyToNatural`


## 0.1.1.1

* Bump base version to work with cabal

## 0.1.1.0

* Package is more organized and now has tests and benchmarks separated
* Repository is more organized and clean
* Added CI

[1]: https://pvp.haskell.org
[2]: https://github.com/bolt12/laop/releases
