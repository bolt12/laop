# How `comp` works

`comp a b` multiplies two matrices: it is the composition `a . b`, which
applies `b` and then `a`. In laop 0.2, `comp` was four equations of the Linear
Algebra of Programming written as Haskell clauses. This page starts from those
clauses and derives the current `comp` from them, one step at a time, and
measures what each step changes. Every version on the page computes the same
numbers, bit for bit, and the last one is the code in `LAoP.Matrix.Internal`.

You need to read Haskell, including GADTs. You do not need to know the Linear
Algebra of Programming.

## Matrices are trees

A matrix has two dimensions, its columns and its rows, and a dimension is a
tree, declared with `type data` because it only exists at the type level:

```haskell
type data Dim = U | Dim :+: Dim
```

`U` has size 1, and `a :+: b` has the size of `a` plus the size of `b`, so
`U :+: U` is 2 and `(U :+: U) :+: (U :+: U)` is 4. A matrix is built from
three constructors:

```haskell
data Matrix e cols rows where
  One  :: e -> Matrix e U U
  Join :: Matrix e a rows -> Matrix e b rows -> Matrix e (a :+: b) rows
  Fork :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (a :+: b)
```

`One x` is a 1 by 1 matrix. `Join l r` puts `l` to the left of `r`, so the
two need the same rows, and `Fork t b` puts `t` above `b`, so the two need the
same columns. Macedo and Oliveira (2013) call a `Join` a junc and a `Fork` a
split, which is where the names of the fusion laws below come from.

The matrix

```
1 2
3 4
```

has type `Matrix Int (U :+: U) (U :+: U)`, and you can build it as two rows,
one above the other, or as two columns side by side:

```haskell
Fork (Join (One 1) (One 2)) (Join (One 3) (One 4))    -- the rows 1 2 and 3 4
Join (Fork (One 1) (One 3)) (Fork (One 2) (One 4))    -- the columns 1 3 and 2 4
```

```
         Fork                       Join
        /    \                     /    \
    Join      Join             Fork      Fork
    /  \      /  \             /  \      /  \
   1    2    3    4           1    3    2    4
```

Both trees are the same matrix, and `==` says so. The type fixes the two
dimension trees but not how the `Join` and `Fork` nodes nest, and we call that
nesting the layout of the matrix. The exchange law (Macedo and Oliveira 2013,
eq. 30) swaps one level of nesting without changing the matrix:

```haskell
Fork (Join a b) (Join c d) == Join (Fork a c) (Fork b d)
```

With `One 1`, `One 2`, `One 3` and `One 4` for `a`, `b`, `c` and `d`, it turns
the left tree above into the right one.

Two layouts have names on this page. A matrix is row-major when every `Fork`
is above every `Join`: a tree of rows, each row a tree of `Join`s. `fromLists`
builds row-major matrices. A matrix is column-major when every `Join` is above
every `Fork`, as `tr` of a row-major matrix is, since `tr` turns each `Join`
into a `Fork` and each `Fork` into a `Join`. Any other layout is mixed, which
is what a matrix put together with `|||` and `===` can have.

The types say one more thing that the later steps rely on. A matrix with one
row, of type `Matrix e cols U`, contains no `Fork`, because a `Fork` has rows
`a :+: b` and never `U`. So a row is a tree of `Join`s over `One`s, and that
tree is exactly the tree `cols`. In the same way, a column is a tree of
`Fork`s whose shape is its row tree.

## The laws

`comp` has the type

```haskell
comp :: Num e => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
```

where `cr` is the dimension the two operands share: the columns of `a` and the
rows of `b`. Four equations say what it does on each constructor. The first is
the product of two numbers, and the other three are laws of Macedo and
Oliveira (2013), where `.+.` adds two matrices of the same size element by
element:

```haskell
comp (One x)    (One y)    == One (x * y)
comp (Join a b) (Fork c d) == comp a c .+. comp b d        -- divide and conquer, eq. 35
comp (Fork a b) c          == Fork (comp a c) (comp b c)   -- split fusion, eq. 27
comp c          (Join a b) == Join (comp c a) (comp c b)   -- junc fusion, eq. 26
```

The two fusion laws say that the rows of a product come from the rows of `a`,
and its columns from the columns of `b`: the top row of `comp a b` is the top
row of `a` times `b`. Divide and conquer splits the shared dimension. An element of the product is a sum over
`cr` of an element of `a` times an element of `b`, and the law adds the sum
over the left subtree of `cr` to the sum over the right subtree.

## Version 0: the laws as a program

With one clause per law, the laws are a program, the `comp` of laop 0.2:

```haskell
comp0 :: Num e => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp0 (One a)    (One b)    = One (a * b)
comp0 (Join a b) (Fork c d) = comp0 a c .+. comp0 b d
comp0 (Fork a b) c          = Fork (comp0 a c) (comp0 b c)
comp0 c          (Join a b) = Join (comp0 c a) (comp0 c b)
```

It is correct because every clause is a law, and it covers every case. Only
two pairs of constructors match no clause, `One` against `Fork` and `Join`
against `One`, and both would need `U` to equal some `a :+: b`, so they do not
type-check. With `-Wall`, GHC reports no missing case.

When more than one clause matches, the first one wins, so the layout of the
operands decides which law runs. Take

```haskell
b = Fork (Join (One 5) (One 6)) (Join (One 7) (One 8))    -- 5 6 / 7 8
```

and the example above as `a`. When `a` is row-major, split fusion runs first,
and divide and conquer then runs on each row of `a`:

```haskell
  comp0 (Fork (Join (One 1) (One 2)) (Join (One 3) (One 4))) b
= Fork (comp0 (Join (One 1) (One 2)) b) (comp0 (Join (One 3) (One 4)) b)   -- eq. 27

  comp0 (Join (One 1) (One 2)) (Fork (Join (One 5) (One 6)) (Join (One 7) (One 8)))
= comp0 (One 1) (Join (One 5) (One 6)) .+. comp0 (One 2) (Join (One 7) (One 8))   -- eq. 35
= Join (One 5) (One 6) .+. Join (One 14) (One 16)                                 -- eq. 26, twice
= Join (One 19) (One 22)
```

Divide and conquer built the rows 5 6 and 14 16 only to add them up. When `a`
is column-major, divide and conquer runs first, on the whole matrix, and adds
two full matrices:

```haskell
  comp0 (Join (Fork (One 1) (One 3)) (Fork (One 2) (One 4))) b
= comp0 (Fork (One 1) (One 3)) (Join (One 5) (One 6))         -- 5 6 / 15 18
    .+. comp0 (Fork (One 2) (One 4)) (Join (One 7) (One 8))   -- 14 16 / 28 32
```

Both orders give 19 22 / 43 50. Every divide-and-conquer step allocates a
temporary block, the size of the part of the result it computes, which `.+.`
reads once and drops. For two n by n matrices the temporaries hold n²(n - 1)
elements in total, whatever the layout. For 4 by 4 matrices that is 12 rows of
4 elements when `a` is row-major, and 3 matrices of 4 by 4 when `a` is
column-major. Large temporaries cost more, because each one stays alive while
the other half of its sum is computed, and the garbage collector copies it.
Multiplying two 500x500 matrices of `Double` takes 1.20 s when `a` is
row-major and 13.3 s when it is column-major, 11.2 s of which go to garbage
collection.

## Version 1: lay the operands out first

Divide and conquer needs a `Join` on the left and a `Fork` on the right. Lay
`a` out row-major and `b` column-major. Then `a` has a `Join` on top only once
it has no `Fork` left, that is once it is a single row, and `b` has a `Fork`
on top only once it is a single column. So divide and conquer only runs on a
row of `a` and a column of `b`, where the block it computes is 1 by 1 and
`.+.` adds two numbers. Every other step is a fusion step, and a fusion step
does no arithmetic: it puts two blocks of the result side by side or one above
the other.

Any matrix can be laid out row-major or column-major with the exchange law,
which does not change the matrix, so for any layouts of `a` and `b`

```haskell
comp a b == comp (rowMajor a) (columnMajor b)
```

Laying out uses `splitFork`, which returns the top and the bottom of a matrix
in any layout:

```haskell
splitFork :: Matrix e c (r1 :+: r2) -> (Matrix e c r1, Matrix e c r2)
splitFork (Fork top bottom)  = (top, bottom)
splitFork (Join left right) =
  let (leftTop, leftBottom)   = splitFork left
      (rightTop, rightBottom) = splitFork right
   in strictPair (Join leftTop rightTop) (Join leftBottom rightBottom)
```

On a `Fork` it takes the two branches. On a `Join` it applies the exchange law
from right to left, as many levels down as the `Join`s go:

```haskell
Join (Fork leftTop leftBottom) (Fork rightTop rightBottom)
  == Fork (Join leftTop rightTop) (Join leftBottom rightBottom)
```

`strictPair` builds both halves before it returns them, so that no half is
left as a suspended computation. On the column-major example `splitFork`
returns the two rows:

```haskell
splitFork (Join (Fork (One 1) (One 3)) (Fork (One 2) (One 4)))
  == (Join (One 1) (One 2), Join (One 3) (One 4))
```

`splitJoin` does the same for the left and the right. `rowMajor` applies
`splitFork` until every block is a single row:

```haskell
rowMajor :: Matrix e cols rows -> Matrix e cols rows
rowMajor m = go (rowShape m) m
  where
    go :: SDim r -> Matrix x c r -> Matrix x c r
    go SU                   x = x
    go (SPlus _ top bottom) x = case splitFork x of
      (xTop, xBottom) -> Fork (go top    xTop)
                              (go bottom xBottom)
```

It needs the row tree at run time to know where to stop, and `rowShape` reads
it off the matrix (`colShape` reads the column tree). `SDim` is a dimension
tree as a value, with one constructor for each constructor of `Dim`:

```haskell
data SDim (d :: Dim) where
  SU    :: SDim U
  SPlus :: Int -> SDim a -> SDim b -> SDim (a :+: b)   -- the Int is the size
```

Matching on `SU` tells the type checker that the rows are `U`, and matching on
`SPlus` that they are a sum, which is what allows the call to `splitFork`.
`columnMajor` is the same with `splitJoin`. On the example, `rowMajor` of the
column-major tree gives the row-major tree, and `columnMajor b` gives
`Join (Fork (One 5) (One 7)) (Fork (One 6) (One 8))`. Laying a matrix out
takes time proportional to its size, and to its number of rows (or columns)
when it is already laid out that way.

Version 1 is version 0 on laid-out operands:

```haskell
comp1 :: Num e => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp1 a b = comp0 (rowMajor a) (columnMajor b)
```

On the example, split fusion runs first, then junc fusion, and divide and
conquer runs last, on a row and a column:

```haskell
  comp0 (Join (One 1) (One 2)) (Fork (One 5) (One 7))
= comp0 (One 1) (One 5) .+. comp0 (One 2) (One 7)    -- eq. 35
= One 5 .+. One 14
= One 19
```

The temporaries are single numbers now. There are as many as before, n²(n - 1),
but each one is dropped as soon as it is added, so the garbage collector has
almost nothing to copy: with a column-major `a`, the 500x500 product takes
1.23 s instead of 13.3 s, and 0.03 s of garbage collection instead of 11.2 s.

## Version 2: a 1 by 1 matrix is a number

On a row and a column, version 1 still wraps every product and every partial
sum in a `One`, with the number boxed inside it: 64 bytes of allocation for
every multiply-add. A 1 by 1 matrix holds exactly one number, so we compute
the number instead. Define `dot` by

```haskell
comp0 r c == One (dot r c)      -- r a row, c a column
```

and calculate its clauses from those of `comp0`. A row and a column over the
same dimension `cr` are either both `One`, or a `Join` and a `Fork`, since a
row has no `Fork` and a column no `Join`. In the first case

```haskell
  comp0 (One x) (One y)
= One (x * y)
```

so `dot (One x) (One y) = x * y`. In the second, assuming the equation for the
smaller rows and columns,

```haskell
  comp0 (Join r1 r2) (Fork c1 c2)
= comp0 r1 c1 .+. comp0 r2 c2              -- eq. 35
= One (dot r1 c1) .+. One (dot r2 c2)      -- the equation, for the halves
= One (dot r1 c1 + dot r2 c2)              -- .+. on 1 by 1 matrices
```

so `dot (Join r1 r2) (Fork c1 c2) = dot r1 c1 + dot r2 c2`. Together:

```haskell
dot :: Num e => Matrix e cr U -> Matrix e U cr -> e
dot (One x)      (One y)      = x * y
dot (Join x1 x2) (Fork y1 y2) = dot x1 y1 + dot x2 y2
```

At `Double`, GHC compiles `dot` to a loop that keeps the partial sum unboxed
and allocates nothing.

The recursion can only call `dot` where the type checker knows that the block
is 1 by 1, and version 1 cannot tell it that. A `Join` on top of `a` shows
that `a` is a single row only because of the layout, and the types do not
record the layout. So the recursion follows the dimension trees, which do
refine the types, instead of the constructors:

```haskell
comp2 :: Num e => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp2 a b = rowsFirst (colShape b) (rowShape a) (rowMajor a) (columnMajor b)

rowsFirst :: Num e => SDim cols -> SDim rows -> Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
rowsFirst cols (SPlus _ top bottom) x y = case splitFork x of     -- eq. 27
  (xTop, xBottom) -> Fork (rowsFirst cols top    xTop    y)
                          (rowsFirst cols bottom xBottom y)
rowsFirst (SPlus _ left right) SU x y = case splitJoin y of        -- eq. 26
  (yLeft, yRight) -> Join (rowsFirst left  SU x yLeft)
                          (rowsFirst right SU x yRight)
rowsFirst SU SU x y = One (dot x y)                                -- eq. 35, as a number
```

Each clause is one law. `splitFork x` only has to take the two branches of a
`Fork`, because the left operand is row-major, and `splitJoin y` the two
branches of a `Join`, because the right one is column-major. The layout only
matters for speed: `splitFork` and `splitJoin` work on any layout, so the
result is the same without it, and slower. Version 2 takes 0.33 s for the 500x500 product, and allocates 25 MB
instead of 8 GB, about the size of the result and of the laid-out operands.

## Version 3: split the longer side first

Version 2 splits all the rows of the result before it splits any column, so
it computes the result one row at a time, and each row reads all of `b`. The
numbers below are the order in which it computes the elements of a 4 by 4
result:

```
 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15 16
```

A laid-out `b` takes 56 bytes per element (the `One`, the boxed number in it,
and about one `Join` or `Fork` node), which is 14 MB for 500x500 and 56 MB
for 1000x1000. Reading all of it once per row is fast while it stays in the
processor's caches, and slow once it no longer fits: version 2 takes 0.33 s
for the 500x500 product and 20.7 s for the 1000x1000 one, 63 times
longer for 8 times the work.

`comp` splits whichever side of the result block is longer, and the rows when
the two are equal, which gives this order for 4 by 4:

```
 1  2  5  6
 3  4  7  8
 9 10 13 14
11 12 15 16
```

A block of k rows and k columns reads k rows of `a` and k columns of `b`, and
uses each of them k times. As the blocks halve, the rows and columns a block
reads come to fit in the cache, whatever its size, with no block size to tune.
Frigo, Leiserson, Prokop and Ramachandran (1999) multiply matrices the same
way and call such algorithms cache-oblivious. Their algorithm halves the
largest of all three dimensions, while `comp` never halves the shared one:
that would be divide and conquer on blocks, with the temporaries that version
1 removed. So a block of `comp` always reads whole rows of `a` and whole
columns of `b`. The 1000x1000 product takes 3.17 s instead of 20.7 s, and
the 500x500 one the same as before.

The order changes the layout of the result and nothing else. Splitting the
columns first gives `Join (Fork ..) (Fork ..)` where splitting the rows first
gives `Fork (Join ..) (Join ..)`, and the exchange law says that these are the
same matrix. This is why the documentation of `comp` leaves the layout of its
result unspecified.

Which side to split is a comparison of two sizes at run time, and each answer
must tell the type checker something different, so the answer is a GADT. Its
fields name the halves of the block:

```haskell
data Split cols rows where
  NoSplit      ::                                 Split U U
  SplitRows    :: !(SDim top)  -> !(SDim bottom) -> Split cols (top :+: bottom)
  SplitColumns :: !(SDim left) -> !(SDim right)  -> Split (left :+: right) rows

splitLongerSide :: SDim cols -> SDim rows -> Split cols rows
splitLongerSide SU                    SU                   = NoSplit
splitLongerSide SU                    (SPlus _ top bottom) = SplitRows top bottom
splitLongerSide (SPlus _ left right)  SU                   = SplitColumns left right
splitLongerSide (SPlus nc left right) (SPlus nr top bottom)
  | nr >= nc  = SplitRows top bottom
  | otherwise = SplitColumns left right
```

A size comparison in a guard on the clauses of `rowsFirst` would make the same
choice, but GHC would then warn about a missing case: it cannot see that the
guard always holds when the columns are `SU` and the rows are not. With
`Split`, the recursion has the three laws of version 2 again. It does not need
to know what it computes from a row and a column, so it takes that as an
argument, and `comp` passes `dot`:

```haskell
comp a b = rowsWithColumns dot (rowMajor a) (columnMajor b)

rowsWithColumns f a b = go (colShape b) (rowShape a) a b
  where
    go cols rows x y = case splitLongerSide cols rows of
      NoSplit                 -> One (f x y)                    -- eq. 35, as a number
      SplitRows top bottom    -> case splitFork x of            -- eq. 27
        (xTop, xBottom) -> Fork (go cols top    xTop    y)
                                (go cols bottom xBottom y)
      SplitColumns left right -> case splitJoin y of            -- eq. 26
        (yLeft, yRight) -> Join (go left  rows x yLeft)
                                (go right rows x yRight)
```

`rowsWithColumns` is exported, for other algorithms with the same shape.
Relational division is one: an element of `divR x y` holds when every element
of a row of `y` implies the element at the same position in a row of `x`,
along the dimension the two relations share. laop 0.2 wrote it with the four clauses of
`comp`, conjunction for addition and implication for multiplication, and it
had their costs. It is now `rowsWithColumns` with an implication in the place
of `dot`, and on 200 by 200 relations it takes 4.6 ms instead of 38 ms, or
6.2 ms instead of 114 ms when the relations are laid out by columns.

## In parallel

The two halves of a fusion step do not depend on each other, so they can be
computed at the same time. `parCompWith depth` repeats the recursion of
`rowsWithColumns` for its first `depth` levels, and those of the layout step,
and builds the two halves of each with

```haskell
inParallel k l r = r `par` (l `pseq` k l r)
```

which sparks the right half and evaluates the left one on the current thread.
Below that depth it is `rowsWithColumns dot`, and with a depth of 0 it is
`comp`. `parComp` chooses the depth from the number of cores and the size of
the product. Each element is still one `dot` of the same row and column, so
every depth gives the same bits. [parallelism.md](parallelism.md) has the
details and the measurements.

## The same numbers, bit for bit

Every version computes each element of the result with the same additions,
grouped the same way, so the results are equal bit for bit, and not only up to
rounding. Fix a row of `a`, with the element `x` at each position of `cr`, and
a column of `b`, with `y` at each position. Their sum over a subtree of `cr` is

```
S(U)       = x * y        -- the elements at that position
S(l :+: r) = S(l) + S(r)
```

`dot` is `S`, clause for clause. In version 0, the fusion laws move blocks
without touching an element, and divide and conquer splits `cr` at its top
node and adds element by element. So whatever order the clauses run in, each
element of the result is `S(cr)` for its row and column. The grouping comes
from the type and not from the layout: a row of type `Matrix e cr U` can only
have the `Join` tree of `cr`, and a column the `Fork` tree.

Floating-point addition is not associative: `(x + y) + z` and `x + (y + z)`
can differ in the last bits. So the grouping matters, and the tests check it.

## How we know it is correct

Every clause of every version is a law, every recursive call is on a smaller
dimension tree, and GHC checks that every version covers every case. The types
also rule out many mistakes. Swapping the two halves in a fusion step, for
instance, is a type error, because the halves have different dimension trees.

`test/Test/Matrix/Composition.hs` keeps version 0 verbatim as `compRef`. It
checks that `comp` returns the same elements, compared as bit patterns, on
operands whose `Join` and `Fork` nodes are nested at random, with balanced and
unbalanced dimension trees, and with `Double` (including zeros of both signs,
infinities and NaN), `Int` and `Boolean` elements. It checks the parallel
`comp` the same way at every depth.

To check the checks, we broke the code on purpose, one change at a time:

| Change | Caught by |
|---|---|
| swap the two halves in the split-fusion branch | type error: `Could not deduce bottom ~ top` |
| swap the two halves in the junc-fusion branch | type error: `Could not deduce right ~ left` |
| in `dot`, pair the left half of the row with the right half of the column | type error: `Could not deduce right ~ left` |
| `dot (One x) (One y) = x * x` | 96 of the 251 tests |
| in `dot`, add three-way sums as `x + (y + z)` | 4 tests, all bit-for-bit `Double` comparisons with version 0 |
| split the columns first, whatever the sizes | no test, correctly: the sums are the same |
| lay out the top half of `a` only | no test, correctly: the layout only changes the speed |

The fourth change computes a wrong product, and the three composition laws
still hold for it, because they say how blocks combine and not what the
product of two numbers is. The tests that catch it compare `comp` with other
definitions: version 0, the `matrix` package, and laws such as identity and
associativity. The fifth change is the reason those comparisons are made bit
for bit. Its results differ from the correct ones by rounding only, and every
other test passed: the laws of the algebra, the `Int` and `Boolean` products,
and the comparison with the `matrix` package.

## What stays algebraic

`comp` satisfies the laws it was derived from, and the test suite checks
divide and conquer and the two fusion laws on it, with the other laws of the
algebra. The rewrite rules in `LAoP.Matrix.Internal`, such as
`comp m iden = m` and `comp p1 (fork a b) = a`, match the calls to `comp` and
do not depend on how it computes, so they apply as they did in laop 0.2.
`comp` is `NOINLINE`, which keeps its calls in the code for the rules to
match. You can reason about `comp` with the laws, as you could in laop 0.2.

The same rules would turn a test of the identity law, `comp iden m == m`, into
`m == m`. The test modules that state laws turn rewrite rules off, and GHC's
specialiser too, because the specialiser applies rules even when they are off.

## The steps measured

Each version multiplying two square matrices of `Double`, with the median of
three products. Both operands are row-major, as `fromLists` builds them,
except in the column where `a` is column-major:

| Version | 500x500 | 500x500, `a` column-major | 1000x1000 | Allocated, 500x500 |
|---|---|---|---|---|
| 0: the laws | 1.20 s | 13.3 s | 13.8 s | 14 GB |
| 1: laid out first | 0.91 s | 1.23 s | 35.4 s | 8.0 GB |
| 2: `dot` | 0.33 s | 0.48 s | 20.7 s | 25 MB |
| 3: longer side first | 0.34 s | 0.42 s | 3.17 s | 25 MB |

Versions 1 and 2 compute the result one row at a time, and at 1000x1000
both take far longer than eight times their time at 500x500. Version 1 is
even slower than version 0 there.

The machine is the one in [parallelism.md](parallelism.md), and each configuration ran in its
own process on the non-threaded runtime. `laop-parallel` reproduces the first
and the last rows, with the variants `old-seq` (version 0) and `seq`, and the
layouts `rows` and `mixed`:

```sh
cabal build laop-parallel
LP=$(cabal list-bin laop-parallel)
$LP old-seq 500 0 mixed 3
$LP seq 1000 0 rows 3
```

Versions 1 and 2 match on `SPlus`, which `LAoP.Matrix.Internal` does not
export, so we measured them by compiling the code on this page against a copy
of the library that exposes `LAoP.Matrix.Internal.Dim`.

## References

- Hugo Daniel Macedo and José Nuno Oliveira. Typing linear algebra: A
  biproduct-oriented approach. Science of Computer Programming 78(11),
  2160-2191, 2013.
  <https://arxiv.org/abs/1312.4818>
- Matteo Frigo, Charles E. Leiserson, Harald Prokop and Sridhar Ramachandran.
  Cache-oblivious algorithms. 40th Annual Symposium on Foundations of Computer
  Science, 1999.
