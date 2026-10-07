# Parallel composition

`parComp` and `parCompWith` compute matrix products on several cores with GHC
sparks. The result is the one the sequential `comp` returns, bit for bit, at
any depth and on any number of cores. On the 16-core machine described below,
a 1000x1000 product of `Double`s runs 10.3 times faster than the sequential
one and a 500x500 product 7.2 times faster, while products of 100x100 and
smaller do not get faster at all.

This page explains how the product is split, what the runtime system does with
the pieces, and where the speedup stops, with the measurement behind each
claim.

## Using it

```haskell
import           LAoP.Matrix.Indexed

result = parComp a b          -- depth chosen from the cores and the size
tuned  = parCompWith 6 a b    -- at most 6 levels of parallel splits
```

`LAoP.Matrix.Nat`, `LAoP.Relation` and `LAoP.Matrix.Internal` export the same
two functions next to their `comp`.

Compile with `-threaded` and run with `+RTS -N` for every core, or `-N8` for
eight. On one capability `parComp` is the sequential product.

## How the product is split

`comp a b` first lays `a` out row by row and `b` column by column, so that a
row of `a` and a column of `b` come apart in constant time. Then it splits the
result in two, along its longer side, and recurses until each block is a
single element: the dot product of a row of `a` and a column of `b`, summed
along the tree of the dimension they share.

The two halves of a split do not depend on each other. `parCompWith d`
evaluates the first `d` levels of splits in parallel, both in the product and
in the step that lays the operands out, with

```haskell
inParallel k l r = r `par` (l `pseq` k l r)
```

which sparks the right half, evaluates the left half on the current thread and
then joins the two. A product at depth `d` creates `3 * (2^d - 1) + 1` sparks:
`2^d - 1` for the result, as many for laying out each operand, and one to lay
the two operands out at the same time.

Only the result is split, never the dimension the product sums over. No partial
products are added up at the end, and every element comes from the same dot
product as in the sequential `comp`. That is why the results are identical, and
why the parallel product needs no memory for partial results.

A spark evaluates its block to weak head normal form. The fields of the matrix
type are strict, so that evaluates every element of the block to its own weak
head normal form, which for `Double`, `Int` and `Boolean` is the whole value.
With an element type whose weak head normal form leaves work undone, a lazy
pair for instance, that work runs later, on whichever thread uses the element.

`parComp` chooses the depth `ceiling (logBase 2 n) + 2` for `n`
capabilities, which gives about four blocks per core, and a smaller depth for
small products, so that each spark has at least about 2^15 multiply-adds of
work. It reads the number of capabilities once, when the program starts; a
program that changes it with `setNumCapabilities` should pass a depth to
`parCompWith`.

## Machine and method

- AMD Ryzen 9 9950X3D: 16 cores and 32 hardware threads, on two dies of 8
  cores. One die has 96 MB of L3 cache and the other 32 MB.
- Linux 6.18, `amd-pstate` in active mode with the `powersave` governor and
  boost enabled. Nothing else ran apart from the machine's usual background
  processes.
- GHC 9.10.3 with cabal's default `-O1`, and the runtime system's defaults: a
  4 MB allocation area per capability (`-A4m`), parallel garbage collection of
  every generation on every capability, and a 20 ms context switch interval.

The table under "Against other libraries" and the relational figure under
"Limits" come from the criterion suite. Every other number comes from
`laop-parallel` (`benchmark/Parallel.hs`), which measures one configuration
per process. It builds two N by N matrices of `Double`s, computes one product
and checks it against the sequential `comp`, then times a few products and
prints the median time with the runtime system's statistics per product: bytes
allocated, GC time and CPU time. `+RTS -s` adds the number of sparks created,
converted, fizzled and garbage-collected. Each configuration ran twice, except
in the layout and small-product tables and at 2000x2000, which ran once, and
the tables give the median of the medians. The last section has the commands.

## Speedup

Time of the sequential `comp` on one capability, and how many times faster
`parComp` at its default depth is with `-N` capabilities:

| Size | Sequential | -N2 | -N4 | -N8 | -N16 | -N24 | -N32 |
|---|---|---|---|---|---|---|---|
| 100x100 | 2.24 ms | 1.0x | 1.1x | 1.0x | 1.0x | 1.0x | 1.0x |
| 200x200 | 18.3 ms | 1.1x | 1.4x | 1.9x | 1.3x | 1.3x | 1.8x |
| 500x500 | 334 ms | 1.9x | 3.2x | 5.2x | 7.2x | 7.8x | 8.7x |
| 1000x1000 | 2.85 s | 2.0x | 3.3x | 6.2x | 10.3x | 10.8x | 10.6x |
| 2000x2000 | 27.9 s |  |  | 6.0x | 10.8x |  | 8.7x |

From 500x500 up the speedup grows with the cores until 16, which is one
hardware thread per core. The second hardware thread of each core (`-N24`,
`-N32`) adds 20% at 500x500 and little at 1000x1000, and at 2000x2000 the
product is slower with it (8.7x against 10.8x). Small products have their own
section below: the typical 200x200 product gains little, and the speedup there
changes a lot from run to run.

## Where the time goes

Per product, at the default depth:

| Size | Cores | Time | Allocated | GC share | Sparks | Converted | Fizzled | GC'd | CPU time |
|---|---|---|---|---|---|---|---|---|---|
| 500x500 | 1 | 334 ms | 26 MB | 5% | 0 | 0 | 0 | 0 | 335 ms |
| 500x500 | 4 | 104 ms | 34 MB | 10% | 52 | 16 | 15 | 20 | 358 ms |
| 500x500 | 16 | 46.4 ms | 39 MB | 11% | 229 | 113 | 34 | 82 | 539 ms |
| 500x500 | 32 | 38.4 ms | 44 MB | 11% | 497 | 248 | 95 | 154 | 813 ms |
| 1000x1000 | 1 | 2.85 s | 104 MB | 3% | 0 | 0 | 0 | 0 | 2.84 s |
| 1000x1000 | 4 | 861 ms | 109 MB | 4% | 47 | 16 | 17 | 14 | 3.07 s |
| 1000x1000 | 16 | 278 ms | 115 MB | 12% | 202 | 110 | 23 | 69 | 3.60 s |
| 1000x1000 | 32 | 270 ms | 121 MB | 13% | 416 | 235 | 54 | 127 | 6.21 s |

A product allocates the result and the new spine of each operand laid out
again, and little else: 26 MB for 500x500, where the algorithm of laop 0.2
allocated 14 GB (see "Layout and shape of the operands"). So allocation and
garbage collection are not what limits the speedup. The 11 to 13% of GC time
with 16 or more capabilities goes away with a larger allocation area, for a
gain of 6 to 9% (see "Runtime system settings").

With 16 or more capabilities about half of the sparks are converted: another
capability took them and evaluated the block. With 4 it is a third. The rest
were evaluated by the thread that created them before anyone took them, and so
were fizzled, or found dead at a garbage collection (GC'd). A spark that is
not converted costs little more than its creation.

The CPU time grows with the cores: 3.60 s on 16 cores for a product that takes
2.84 s alone. Part of the difference is work done twice (next section), and
the rest is scheduling and parallel garbage collection. With 32 capabilities
the idle ones also spin while they look for work.

The measurements point at memory access as the limit that remains. Every
element is a boxed value behind a pointer, and a dot product walks one row and
one column through those pointers. A 1000x1000 product takes 26% longer when
both operands are read in strided order instead of one (see "Layout and shape
of the operands"), and from 1000x1000 up hardware threads beyond one per core
do not help.

### Work done twice

When the thread that sparked a block reaches it and another capability is
still evaluating it, the thread does not wait: by default GHC marks a thunk
under evaluation only lazily, so the thread evaluates the block again. The
spark counts show it. A product at depth `d` should create exactly
`3 * (2^d - 1) + 1` sparks, and it does with `-feager-blackholing`, which marks
every thunk as soon as its evaluation starts. With the default lazy
blackholing it creates more, and each extra spark comes from a block that was
evaluated twice.

| Size | Cores | Expected sparks | Lazy (default) | Eager | Time, lazy | Time, eager |
|---|---|---|---|---|---|---|
| 200x200 | 4 | 46 | 52 | 46 | 9.99 ms | 14.2 ms |
| 200x200 | 8 | 94 | 120 | 94 | 7.15 ms | 16.1 ms |
| 200x200 | 16 | 190 | 232 | 190 | 11.4 ms | 16.7 ms |
| 200x200 | 32 | 190 | 212 | 190 | 13.2 ms | 15.4 ms |
| 500x500 | 4 | 46 | 50 | 46 | 104 ms | 98.1 ms |
| 500x500 | 8 | 94 | 110 | 94 | 64.4 ms | 53.5 ms |
| 500x500 | 16 | 190 | 233 | 190 | 46.1 ms | 33.9 ms |
| 500x500 | 32 | 382 | 517 | 382 | 38.6 ms | 28.3 ms |
| 1000x1000 | 4 | 46 | 47 | 46 | 883 ms | 817 ms |
| 1000x1000 | 8 | 94 | 96 | 94 | 477 ms | 465 ms |
| 1000x1000 | 16 | 190 | 197 | 190 | 293 ms | 281 ms |
| 1000x1000 | 32 | 382 | 417 | 382 | 268 ms | 267 ms |

Eager blackholing removes the duplicated work, and a 500x500 product runs 26%
faster with it on 16 or 32 capabilities. A 200x200 product, though, runs up to
2.3 times slower. With eager blackholing a thread that reaches a block under
evaluation blocks until the other capability finishes it and wakes the thread,
and for a 200x200 product that wait costs more than evaluating the block again
(where exactly the time goes was not traced further). The library is compiled
without it. To try it, build with `--ghc-options=-feager-blackholing`, which
applies it to the library as well: the sparked thunks are created in
`LAoP.Matrix.Internal`.

## Depth

Median time of `parCompWith d` at each depth, fastest in bold, the default
depth marked with `*`:

500x500:

| Cores | d=0 | d=1 | d=2 | d=3 | d=4 | d=5 | d=6 | d=7 | d=8 | d=9 |
|---|---|---|---|---|---|---|---|---|---|---|
| 4 | 330 ms | 170 ms | **95.2 ms** | 96.4 ms | 113 ms * | 106 ms | 105 ms | 111 ms | 108 ms | 104 ms |
| 8 | 342 ms | 172 ms | 100 ms | 78.8 ms | **64.8 ms** | 65.1 ms * | 68.0 ms | 80.8 ms | 78.9 ms | 84.4 ms |
| 16 | 335 ms | 175 ms | 100 ms | 72.9 ms | 48.1 ms | **44.5 ms** | 48.8 ms * | 51.6 ms | 54.6 ms | 61.4 ms |
| 32 | 330 ms | 173 ms | 102 ms | 67.0 ms | 47.7 ms | 37.8 ms | **35.7 ms** | 40.2 ms * | 44.2 ms | 52.0 ms |

1000x1000:

| Cores | d=0 | d=1 | d=2 | d=3 | d=4 | d=5 | d=6 | d=7 | d=8 | d=9 |
|---|---|---|---|---|---|---|---|---|---|---|
| 4 | 2.89 s | 1.44 s | 795 ms | 806 ms | **794 ms** * | 892 ms | 882 ms | 876 ms | 834 ms | 827 ms |
| 8 | 2.77 s | 1.46 s | 767 ms | **459 ms** | 463 ms | 491 ms * | 481 ms | 469 ms | 498 ms | 521 ms |
| 16 | 2.82 s | 1.45 s | 776 ms | 429 ms | 325 ms | 304 ms | 288 ms * | **281 ms** | 298 ms | 335 ms |
| 32 | 2.83 s | 1.45 s | 771 ms | 449 ms | 335 ms | 288 ms | 280 ms | 279 ms * | **265 ms** | 282 ms |

Each level of depth halves the blocks, and the time falls until there are
about as many blocks as cores (depth 2 on 4 cores, 4 on 16), changes little
over the next three or four levels while the extra blocks balance the load,
then rises as the sparks and the work done twice add up. The default,
`ceiling (logBase 2 n) + 2`, is within 7% of the fastest depth at 1000x1000
and within 19% at 500x500, where one level less would do slightly better. The
deeper default pays off on smaller products: a 200x200 product on 16 cores
took 6.1 ms at depth 6 and 14 ms at depth 5.

## Small products

Sequential time, and the median time of `parComp` on 16 cores at the default
depth and at other depths:

| Size | Sequential | Default depth | d=1 | d=2 | d=3 | d=4 | d=6 |
|---|---|---|---|---|---|---|---|
| 10x10 | 5 µs | 5 µs | 5 µs | 5 µs | 5 µs | 5 µs | 6 µs |
| 50x50 | 286 µs | 273 µs | 275 µs | 273 µs | 284 µs | 279 µs | 282 µs |
| 100x100 | 2.16 ms | 2.22 ms | 2.20 ms | 2.16 ms | 2.25 ms | 2.21 ms | 2.25 ms |
| 150x150 | 7.21 ms | 5.12 ms | 7.12 ms | 6.19 ms | 6.88 ms | 4.85 ms | 5.34 ms |

Below 64x64 the default depth is 0 and `parComp` creates no sparks. At
100x100 it creates some, and the median product still takes as long as the
sequential one, although the fastest of the timed products took 0.83 ms against
2.24 ms. The sparks are there; the idle capabilities pick them up too late.

A spark goes into the pool of the capability that created it, and a capability
that has run out of work sleeps until the runtime system wakes it. These
products allocate little, so they seldom stop for a garbage collection. A
smaller allocation area makes collections more frequent and doubles the share
of sparks that another capability runs, while a faster timer tick does not:

| Size | Setting, -N16 | Time | Sparks converted |
|---|---|---|---|
| 150x150 | sequential | 7.65 ms | |
| 150x150 | defaults | 6.01 ms | 21% |
| 150x150 | `-A1m` | 2.57 ms | 45% |
| 150x150 | `-V0.001` (1 ms tick) | 5.94 ms | 20% |
| 200x200 | sequential | 19.1 ms | |
| 200x200 | defaults | 16.0 ms | 20% |
| 200x200 | `-A1m` | 5.42 ms | 43% |
| 200x200 | `-V0.001` (1 ms tick) | 8.88 ms | 24% |

If your program multiplies matrices of this size in parallel, run it with
`+RTS -A1m`. From 500x500 up it makes little difference.

## Runtime system settings

Median time on 16 cores:

| Setting | 500x500 | GC share | 1000x1000 | GC share |
|---|---|---|---|---|
| defaults (`-A4m`) | 45.5 ms | 12% | 280 ms | 14% |
| `-A1m` | 43.9 ms | 20% | 284 ms | 9% |
| `-A16m` | 42.4 ms | 7% | 282 ms | 4% |
| `-A64m` | 41.6 ms | 0% | 264 ms | 0% |
| `-A256m` | 42.0 ms | 0% | 263 ms | 0% |
| `-qn8` (8 GC threads) | 43.6 ms | 14% | 300 ms | 14% |
| `-qn2` (2 GC threads) | 46.8 ms | 20% | 311 ms | 19% |
| `-qg` (sequential GC) | 47.8 ms | 23% | 304 ms | 27% |
| `-qa` (pinned threads) | 42.5 ms | 7% | 289 ms | 20% |

- A 64 MB allocation area per capability (`-A64m`) removes garbage collection
  from the product and gains 6 to 9%.
- Keep the parallel garbage collector on all capabilities: with fewer GC
  threads, or none, the collection takes longer and so does the product.
- Pinning threads to cores (`-qa`) does not help consistently.

## Cores, hyperthreads and caches

The CPUs a run may use were set with `taskset`. Linux numbers the first
hardware thread of each core 0 to 15 and the second 16 to 31, and the die with
96 MB of L3 holds cores 0 to 7.

| Threads | CPUs | 500x500 | 1000x1000 |
|---|---|---|---|
| 8 | 8 cores of the 96 MB L3 die | 61.1 ms | 409 ms |
| 8 | 8 cores of the 32 MB L3 die | 67.1 ms | 485 ms |
| 16 | 8 cores, both hardware threads | 55.7 ms | 428 ms |
| 16 | 16 cores, one thread each | 46.7 ms | 269 ms |
| 16 | any (the scheduler's choice) | 50.8 ms | 282 ms |
| 32 | 16 cores, both hardware threads | 38.5 ms | 270 ms |

A second core is worth much more than a second hardware thread: 16 threads on 8
cores are about as fast as 8 threads on 8 cores, and 16 threads on 16 cores
are 1.6 times as fast at 1000x1000. The die with the larger cache was 9 to 16%
faster in these runs. Leaving the placement to the operating system costs 5 to
8% against one thread per core.

## Layout and shape of the operands

`fromLists` builds a matrix row by row, `tr` turns that into one laid out
column by column, and `join` builds a matrix from blocks of columns, so the
two operands of a product can come in any layout. Sequential and 16-core
times, with the algorithm of laop 0.2 and its parallel version at depth 4 for
comparison:

| Size | Operands | Sequential | -N16 | Allocated | 0.2 sequential | 0.2 at -N16 | 0.2 allocated |
|---|---|---|---|---|---|---|---|
| 500x500 | both by rows | 341 ms | 42.4 ms | 26 MB | 1.13 s | 211 ms | 13973 MB |
| 500x500 | both by columns | 340 ms | 52.3 ms | 26 MB | | | |
| 500x500 | left by columns, right by rows | 392 ms | 52.6 ms | 38 MB | 12.97 s | 4.89 s | 13986 MB |
| 500x500 | split 1:9 at the top | 311 ms | 40.7 ms | 26 MB | | | |
| 1000x1000 | both by rows | 2.86 s | 302 ms | 104 MB | 13.06 s | 1.89 s | 111894 MB |
| 1000x1000 | both by columns | 2.76 s | 295 ms | 104 MB | | | |
| 1000x1000 | left by columns, right by rows | 3.61 s | 365 ms | 152 MB | | | |
| 1000x1000 | split 1:9 at the top | 2.79 s | 343 ms | 104 MB | | | |

The algorithm of laop 0.2 first split the dimension the product sums over, so
it built whole partial products and added them up, and its parallel version
split that dimension in parallel too. With a left operand laid out by columns,
every level held a full partial product: the 500x500 product spent 84% of its
13 s in garbage collection, and the parallel version held a gigabyte of live
data. The current algorithm lays both operands out first and costs the same
for every layout, apart from the extra reads when both are strided.

The last shape has every dimension split 1:9 at the top, as a matrix indexed by
`Either A B` with `B` nine times the size of `A`. The sequential time is the
same. On 16 cores the 500x500 product is as fast as with balanced trees and the
1000x1000 one 14% slower, since the first split of the result gives one small
and one large half.

## Against other libraries

From the criterion suite (`laop-benchmark`, group `CrossLibrary`), on the same
inputs converted to each library's type. With one capability, hmatrix with
`OMP_NUM_THREADS=1`:

| Size | LAoP `comp` | hmatrix | Data.Matrix | linear |
|---|---|---|---|---|
| 100x100 | 2.33 ms | 27 µs | 3.74 ms | 18.3 ms |
| 200x200 | 19.6 ms | 201 µs | 32.1 ms | |
| 500x500 | 339 ms | 3.07 ms | 525 ms | |
| 1000x1000 | 2.94 s | 24.4 ms | 4.89 s | |

With 16 capabilities, hmatrix with `OMP_NUM_THREADS=16` (Data.Matrix has no
parallel product):

| Size | LAoP `parComp` | hmatrix | Data.Matrix |
|---|---|---|---|
| 100x100 | 2.14 ms | 30 µs | 3.93 ms |
| 200x200 | 9.78 ms | 56 µs | 34.3 ms |
| 500x500 | 46.4 ms | 442 µs | 561 ms |
| 1000x1000 | 287 ms | 2.24 ms | 7.39 s |

hmatrix multiplies unboxed arrays with BLAS and is 85 to 120 times faster than
LAoP on one core, and 70 to 175 times faster on 16. LAoP computes on a tree of
boxed elements so that its algorithms can follow the laws of the algebra, and
it is not a replacement for a numerical library. Against Data.Matrix, which
also stores boxed elements, sequential LAoP is 1.5 to 1.7 times faster, and at
1000x1000 `parComp` on 16 cores is 17 times faster than Data.Matrix on one.

## Limits

- `comp` and `parComp` are specialised for `Double`, `Int` and `Boolean`
  elements. At another element type, or when called from code that is
  polymorphic in the element type, they run a generic version that boxes a
  partial sum at every multiply-add and is four to five times slower. A
  `SPECIALISE` or `INLINABLE` pragma on the calling code gets the specialised
  one.
- Products under 64x64 are not split, and up to about 200x200 the speedup
  depends on how soon idle capabilities wake up (see "Small products").
- A product whose result is a single element, a row times a column, is one dot
  product and is not split at all.
- The shape of the dimensions decides the blocks: a very uneven tree, such as a
  long chain of `Either ()`, gives uneven blocks.
- A `Boolean` dot product stops at its first true term, so the work in a block
  of a relational product depends on the data. The criterion group
  `Parallel/relation` has 500x500 relations with half the entries true taking
  22.5 ms sequentially and 10.0 ms on 16 cores.

## Reproducing the tables

`laop-parallel` takes the variant, the size, the depth, the layout and the
number of timed products, and prints one line of comma-separated values; see
the header of `benchmark/Parallel.hs` for the columns.

```sh
cabal build laop-parallel
LP=$(cabal list-bin laop-parallel)

# One configuration: 1000x1000 at the default depth on 16 capabilities.
$LP default 1000 0 rows 4 +RTS -N16 -s

# The speedup table.
for n in 100 200 500 1000; do
  case $n in 100) k=40;; 200) k=20;; 500) k=10;; 1000) k=4;; esac
  $LP seq $n 0 rows $k +RTS -N1
  for N in 2 4 8 16 24 32; do $LP default $n 0 rows $k +RTS -N$N -s; done
done

# The depth table: variant par with depths 0 to 9.
$LP par 500 5 rows 10 +RTS -N16 -s

# Runtime system settings: add them after the -N flag.
$LP default 200 0 rows 20 +RTS -N16 -A1m -s

# Layouts: rows, cols, mixed or skew; old-seq and old-par for laop 0.2.
$LP old-par 500 4 mixed 3 +RTS -N16 -s

# Placement.
taskset -c 0-7 $LP default 1000 0 rows 4 +RTS -N8

# Eager blackholing, in a separate build directory.
cabal build laop-parallel --builddir=dist-newstyle/eager --ghc-options=-feager-blackholing
```

The criterion suite has the same comparisons in the groups
`Parallel/default`, `Parallel/depth-sweep`, `Parallel/relation` and
`CrossLibrary`. It runs on one capability unless told otherwise:

```sh
OMP_NUM_THREADS=16 cabal bench laop-benchmark --benchmark-options='CrossLibrary +RTS -N16'
```
