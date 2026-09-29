# Changelog

`laop` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.3.0.0

Matrix dimensions have a new representation. Most operations keep their names
and meaning, but code that names a dimension constraint or defines its own
index types needs changes. Requires GHC 9.10 or later and is tested with GHC
9.10.3. New dependencies: `array` and `parallel`.

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
- Dimensions are the closed kind `Dim = U | Dim :+: Dim` instead of nested
  `Either` and `()`. `FromNat n` still maps a natural to a balanced tree.
- Functions ask for `MatIndex a` in `LAoP.Matrix.Indexed`, `LAoP.Relation`
  and `LAoP.Dist`, and for `KnownNat n` in `LAoP.Matrix.Nat`.
- `LAoP.Matrix.Nat.fromF` and `cond` take functions on zero-based indices
  (`Int -> Int` and `Int -> Bool`) instead of functions on `Enum` types.
- The `LAoP.Matrix.Nat` block operations (`join`, `fork`, `p1`, `p2`, `i1`,
  `i2`, `-|-`, `select`, `kr`, `fstM`, `sndM`, `><`) accept any sizes. When
  the blocks do not line up with the balanced tree of the result, the result is
  laid out again in O(size).
- `Natural` is renamed `Ranged`, `reifyToNatural` is `mkRanged` and
  `coerceNat*` are `coerceRanged*`. The old names remain as deprecated aliases.
  Match values with the `Rng` pattern and build them with `mkRanged`, a
  numeric literal or `toEnum`; the constructor is no longer exported.
- `mkRanged`, `fromInteger`, `read`, `coerceRanged` and `coerceRanged2` check
  the range and report the value that failed, and `[x ..]` stops at
  `maxBound`.
- Relations hold `Boolean`, the semiring where `+` is or and `*` is and,
  instead of `Natural 0 1`, and print as `0` and `1`. `Relation` and `Dist`
  wrap `LAoP.Matrix.Indexed` matrices, so relational composition is matrix
  composition.
- `Matrix` and `Dist` have no `Num` instance. Use `.+.`, `.-.` and `.*.` for
  element-wise arithmetic.
- `Ord` on matrices compares the elements in row-major order. The 0.2 instance
  was an element-wise partial order that broke the `Ord` laws; use
  `LAoP.Relation.sse` for inclusion.
- `.` from `LAoP.Utils` is right-associative, like `Prelude..`. Chains of
  products group differently, which can change the last bits of a `Double`
  result.
- `fromLists`, `col` and `row` check both dimensions and report the lengths
  they got.
- `kr`, `><`, `-|-`, `select` and `branch` only need `Num e` and follow the
  block structure instead of multiplying by projection matrices, so they run
  in time proportional to the result.

### Added

- `LAoP.Matrix.Parallel`, `LAoP.Matrix.Parallel.Indexed` and
  `LAoP.Matrix.Parallel.Nat`: `comp` sparks independent sub-products, and
  `compWith` takes the depth cutoff. See `PARALLELISM.md`.
- Rewrite rules for the identity, transpose, zero, one and biproduct laws,
  such as `comp m iden = m` and `comp p1 (fork a b) = a`. They assume exact
  arithmetic, so with `NaN` or infinities an optimised build can return a
  different result from an unoptimised one. The parallel `comp` is never
  rewritten.
- A `MatIndex` instance for `BoundedList`, so `pt` and `belongs` no longer
  need `Enum` or `Bounded`.
- `splitJoin` and `splitFork` in `LAoP.Matrix.Internal`, `LAoP.Matrix.Indexed`
  and `LAoP.Matrix.Nat` take a matrix apart into its column or row blocks,
  whatever its layout.
- `LAoP.Dist.fromFreqs` and `(??)`, `LAoP.Relation.complement`, `difference`
  and `fromRel`, `LAoP.Matrix.Indexed.cardinality`, `select` and `branch`,
  `LAoP.Matrix.Nat.point`, and a `Category` instance for `LAoP.Matrix.Nat`
  matrices.

### Removed

- The constraint synonyms (`Countable*`, `FL*`, `Liftable`, `Trivial*`), the
  `Count` and `Normalize` type families, the `FromLists` class and the `Zero`
  type.
- `fromF'` from every module (use `fromF`), and `fmapM`, `returnM`, `bindM`,
  `columns'` and `rows'` from the matrix module.
- `LAoP.Dist.branchD` and `ifD`.
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
