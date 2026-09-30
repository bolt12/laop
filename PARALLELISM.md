# Parallel matrix composition

`LAoP.Matrix.Parallel` multiplies matrices with GHC sparks. On the machine
below, a 500x500 product runs 7.9 times faster than the sequential `comp` with
16 threads. This page has the measurements, the tuning advice, and a section on
the rewrite rules, which remove some products before they run.

## Quick start

```haskell
import           LAoP.Matrix.Indexed
import qualified LAoP.Matrix.Parallel.Indexed as Par

result = Par.comp a b          -- depth cutoff 4
tuned  = Par.compWith 6 a b    -- explicit cutoff
```

`LAoP.Matrix.Parallel` works on the raw matrix type and
`LAoP.Matrix.Parallel.Nat` on `LAoP.Matrix.Nat` matrices. Compile with
`-threaded -rtsopts` and run with `+RTS -N`.

## How it works

Every `Fork` or `Join` node splits a product into two independent products.
`compWith` sparks one of them with `par` and evaluates the other on the
current thread:

```haskell
compWith d (Fork a b) c =
  let l = compWith (d - 1) a c
      r = compWith (d - 1) b c
  in  r `par` l `pseq` Fork l r
```

A spark evaluates its argument to weak head normal form. Every field of the
matrix type is strict, so that evaluates the whole sub-product. Below the depth
cutoff, and for any depth of 0 or less, `compWith` calls the sequential `comp`.

## Setup

- AMD Ryzen 9 9950X3D, 16 cores and 32 threads, Linux 6.18.
- GHC 9.10.3 with cabal's default `-O1`.
- hmatrix linked against OpenBLAS 0.3.33. This build of OpenBLAS takes its
  thread count from `OMP_NUM_THREADS`; `OPENBLAS_NUM_THREADS` alone does not
  pin it.
- Criterion, with inputs built in `env` and results forced with `nf`.

Every number comes from the `laop-benchmark` suite, for example
`cabal bench --benchmark-options='Parallel/default +RTS -N8'`.

## Speedup

Time of `comp` divided by time of `Par.comp`, both measured at the same `-N`.
The sequential time also grows with `-N` (1.33 s at `-N1`, 1.59 s at `-N16`
for 500x500), so comparing at the same `-N` isolates the effect of the sparks.

| Size      | -N1   | -N2   | -N4   | -N8   | -N16  | -N32  |
|-----------|-------|-------|-------|-------|-------|-------|
| 100x100   | 1.03x | 1.83x | 3.02x | 3.62x | 4.96x | 4.95x |
| 200x200   | 0.99x | 1.86x | 3.33x | 4.68x | 6.26x | 6.62x |
| 500x500   | 0.99x | 1.87x | 2.99x | 4.01x | 7.86x | 7.01x |
| 1000x1000 | 0.99x | 1.88x | 3.56x | 4.73x | 8.47x | 8.25x |

At `-N1` the parallel version costs nothing measurable. Efficiency (speedup
divided by cores) is 75 to 90% at 4 cores and about 50% at 16 for 500x500 and
larger. The second hardware thread of each core (`-N32`) adds nothing at those
sizes and little below them.

## Spark statistics

Counts from `+RTS -s` for the 500x500 product at the default depth, summed
over all benchmark iterations:

| Cores | Sparks | Converted  | Fizzled |
|-------|--------|------------|---------|
| -N2   | 242    | 18 (7%)    | 224     |
| -N4   | 247    | 113 (46%)  | 130     |
| -N8   | 242    | 202 (83%)  | 40      |
| -N16  | 242    | 242 (100%) | 0       |

At `-N2` the main thread reaches most sparks before the second worker does, so
they fizzle. The few that convert each carry a whole sub-product, which is why
`-N2` still gives 1.87x.

## Against other libraries

Matrix multiplication on identical inputs, built as LAoP matrices and
converted inside `env`.

Single-threaded: LAoP at `-N1`, hmatrix with `OMP_NUM_THREADS=1`.

| Size      | LAoP `comp` | hmatrix | Data.Matrix | linear  |
|-----------|-------------|---------|-------------|---------|
| 100x100   | 9.58 ms     | 27 µs   | 3.76 ms     | 17.9 ms |
| 200x200   | 79.9 ms     | 201 µs  | 32.0 ms     |         |
| 500x500   | 1.33 s      | 3.08 ms | 528 ms      |         |
| 1000x1000 | 14.6 s      | 24.5 ms | 4.97 s      |         |

With 16 threads: LAoP `Par.comp` at `-N16`, hmatrix with
`OMP_NUM_THREADS=16`. Data.Matrix has no parallel multiplication.

| Size      | LAoP `Par.comp` | hmatrix | Data.Matrix |
|-----------|-----------------|---------|-------------|
| 100x100   | 2.21 ms         | 29 µs   | 4.07 ms     |
| 200x200   | 13.8 ms         | 55 µs   | 35.6 ms     |
| 500x500   | 206 ms          | 404 µs  | 550 ms      |
| 1000x1000 | 1.99 s          | 2.40 ms | 7.75 s      |

hmatrix is 350 to 600 times faster than sequential LAoP. It multiplies dense
arrays with BLAS, while LAoP walks a tree of boxed elements, so LAoP is not a
substitute for a numerical library. Sequentially Data.Matrix is 2.5 to 3 times
faster than LAoP; with 16 threads `Par.comp` is 1.8 to 3.9 times faster than
Data.Matrix, more so on larger matrices.

## Rewrite rules

`LAoP.Matrix.Internal` and `LAoP.Matrix.Indexed` each carry 18 rules:

- identity: `comp m iden = m`, `comp iden m = m`
- transpose: `tr (tr m) = m`, `tr iden = iden`
- addition: `m .+. zeros = m`, `zeros .+. m = m`
- Hadamard product: `m .*. ones = m`, `ones .*. m = m`, `m .*. zeros = zeros`,
  `zeros .*. m = zeros`
- biproduct and orthogonality: `comp p1 i1 = iden`, `comp p2 i2 = iden`,
  `comp p1 i2 = zeros`, `comp p2 i1 = zeros`
- cancellation: `comp p1 (fork a b) = a`, `comp p2 (fork a b) = b`,
  `comp (join a b) i1 = a`, `comp (join a b) i2 = b`

The group names follow Macedo and Oliveira (2013): biproduct is their eqs. 11
and 12, orthogonality 14 and 15, cancellation 28 and 29.

The rules fire in optimised builds (`-O`), including on the operator spellings
`===` and `|||` and on `.` from `LAoP.Utils`. The parallel `comp` is never
rewritten. The rules assume exact arithmetic: a matrix holding `NaN` or an
infinity can give a different result with optimisation than without it,
because the rewritten program skips the products by zero.

A single rule, `comp m iden`, against the same product through a wrapper the
rules cannot see:

| Size    | Rule fires | No rule |
|---------|------------|---------|
| 100x100 | 23.8 µs    | 11.8 ms |
| 500x500 | 602 µs     | 1.58 s  |

With the rule the benchmark only walks the returned matrix to force it.

### Fusion pipeline

"Fusion" here means what GHC's rewriting does to the pipeline. None of the
steps is one of the fusion laws of Macedo and Oliveira (2013, eqs. 26 and 27);
`comp` applies those by definition. Eight steps, each matching one rule.
Together they reduce to the first input:

```haskell
fusionPipeline a b =
  let s1 = comp p1 (fork a b)        -- comp/p1-fork
      s2 = comp s1 iden              -- comp/iden-right
      s3 = tr (tr s2)                -- tr/involution
      s4 = s3 .+. zeros              -- add/zeros-right
      s5 = s4 .*. ones               -- had/ones-right
      s6 = comp (join s5 b) i1       -- comp/join-i1
      s7 = comp iden s6              -- comp/iden-left
      s8 = comp p2 (fork b s7)       -- comp/p2-fork
  in s8
```

The rules do what a person simplifying the expression by hand would do. The
comparison that matters is between running every step and running the
simplified program, in each library. Single-threaded throughout:

| Size    | LAoP, rules | LAoP, every step | hmatrix, every step | hmatrix, simplified |
|---------|-------------|------------------|---------------------|---------------------|
| 100x100 | 22.5 µs     | 96.6 ms          | 406 µs              | 6 ns                |
| 200x200 | 90.1 µs     | 749 ms           | 3.34 ms             | 6 ns                |
| 500x500 | 606 µs      |                  | 39.5 ms             | 6 ns                |

Neither simplified version does any arithmetic. The LAoP time is the walk that
forces the returned tree; the hmatrix one returns an array that is already
evaluated. The rules pay off in code where these patterns come out of
composition, for example a projection applied to a pair of matrices built
elsewhere, rather than in code a person would write in simplified form anyway.

## Tuning

`Par.comp` uses depth 4. Measured with `Parallel/depth-sweep`:

| Size    | Cores | depth 1 | depth 3 | depth 5 | depth 7 | sequential |
|---------|-------|---------|---------|---------|---------|------------|
| 200x200 | -N4   | 41.8 ms | 22.7 ms | 23.1 ms | 22.6 ms | 79.0 ms    |
| 500x500 | -N4   |         | 445 ms  | 449 ms  | 446 ms  | 1.37 s     |
| 200x200 | -N16  | 46.2 ms | 18.6 ms | 14.8 ms | 12.4 ms | 88.8 ms    |
| 500x500 | -N16  |         | 302 ms  | 222 ms  | 190 ms  | 1.57 s     |

At 4 cores depth 3 is enough. At 16 cores deeper cutoffs keep helping, and
depth 7 beats the default by about 6% on 500x500.

Leave the allocation area at its default: at `-N16` the 500x500 product took
208 ms by default and 532 ms with `-A64M`.

```
+RTS -N        -- use all available cores
+RTS -N8 -s    -- 8 threads, print spark statistics
```
