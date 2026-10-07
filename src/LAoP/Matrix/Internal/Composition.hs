{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Composition
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrix composition, as one recursion over the blocks of the result
('rowsWithColumns') and one over a row and a column ('dot').
-}
module LAoP.Matrix.Internal.Composition (
  comp,
  parComp,
  parCompWith,
  rowsWithColumns,
  rowMajor,
  columnMajor,
  dot,
) where

import           Control.Parallel                    (par, pseq)
import           Data.Bits                           (countLeadingZeros,
                                                      finiteBitSize)
import           GHC.Conc                            (numCapabilities)
import           LAoP.Matrix.Internal.Boolean        (Boolean)
import           LAoP.Matrix.Internal.Construction   (iden)
import           LAoP.Matrix.Internal.Dim
import           LAoP.Matrix.Internal.Representation (Matrix (..), colShape,
                                                      columns', rowShape, rows',
                                                      splitFork, splitJoin)
{- | Matrix composition: @comp a b@ is the product @a . b@, the matrix that
applies @b@ and then @a@. Each element of the result is a row of @a@ times a
column of @b@: the sum, over the dimension they share, of the products of
their elements.

Four laws of Macedo and Oliveira (2013) specify it, and laop 0.2 used them as
its definition, one clause each:

@
comp (One x)    (One y)    == One (x * y)
comp (Join a b) (Fork c d) == comp a c .+. comp b d        -- divide and conquer, eq. 35
comp (Fork a b) c          == Fork (comp a c) (comp b c)   -- split fusion, eq. 27
comp c          (Join a b) == Join (comp c a) (comp c b)   -- junc fusion, eq. 26
@

Run as they stand, the clauses spend their time in divide and conquer, which
builds blocks of partial sums only to add them up. 'comp' applies the same laws
in another order. It first lays @a@ out row-major and @b@ column-major
('rowMajor', 'columnMajor'), which the exchange law allows without changing
either matrix. It then applies the two fusion laws, which compute nothing,
until each block of the result is a single element ('rowsWithColumns'). Divide
and conquer then only ever meets a row and a column, whose product is a number
('dot'). Each element is the same sum, grouped the same way, as with the four
clauses, so the results are equal bit for bit.

An @r@ by @k@ matrix times a @k@ by @c@ one costs O(r * k * c) time and
allocates only the result and the two laid-out operands. How the result nests
its 'Join' and 'Fork' nodes is unspecified.

'comp' is specialised for 'Double', 'Int' and t'Boolean' elements. At another
element type, or when called from code that is polymorphic in the element
type, it runs a generic version that allocates a boxed partial sum for every
multiply-add and is about four times slower. Specialise the calling code (a
@SPECIALISE@ or @INLINABLE@ pragma) to reach the fast one.

Optimised builds rewrite some compositions away with the rewrite rules listed in
"LAoP.Matrix.Internal#rules" (@comp m iden@ becomes @m@, @comp p1 (fork a b)@
becomes @a@, and so on). The rules assume exact arithmetic: if a matrix holds
@NaN@ or an infinity, the result can differ from an unoptimised build, because
the skipped products by zero would have propagated it.
-}
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp a b = rowsWithColumns dot (rowMajor a) (columnMajor b)
{-# NOINLINE comp #-}
-- Phase [0], after the rewrite rules on 'comp' have had their chance.
{-# SPECIALISE [0] comp :: Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}
{-# SPECIALISE [0] comp :: Matrix Int cr rows -> Matrix Int cols cr -> Matrix Int cols rows #-}
{-# SPECIALISE [0] comp :: Matrix Boolean cr rows -> Matrix Boolean cols cr -> Matrix Boolean cols rows #-}

{- | @rowsWithColumns f a b@ is the matrix whose element in row @i@ and column
@j@ is @f@ applied to row @i@ of @a@ and column @j@ of @b@. 'comp' is
@rowsWithColumns 'dot'@, and relational division uses it with implication.

It builds the result block by block with the fusion laws of Macedo and
Oliveira (2013, eqs. 26 and 27), halving the longer side of each block until
the block is a single element:

@
comp (fork a1 a2) b == fork (comp a1 b) (comp a2 b)   -- rows: top and bottom halves
comp a (join b1 b2) == join (comp a b1) (comp a b2)   -- columns: left and right halves
@

The recursion follows the dimension trees of the result, as 'SDim' values,
rather than the constructors of @a@ and @b@. Matching on a tree tells the type
checker when a block is a single element, so that @f@ can be called on a row
and a column; a 'Join' on top of @a@ cannot say so, because the layout is not
part of the type. The sizes cached in the trees choose the longer side, which
keeps the blocks square: a block of @k@ rows and @k@ columns reads @k@ rows of
@a@ and @k@ columns of @b@ and uses each of them @k@ times, so they stay in
cache.

It works on operands in any layout, and is fastest on a row-major @a@ and a
column-major @b@ ('rowMajor', 'columnMajor'), whose rows and columns come apart
in constant time.
-}
rowsWithColumns ::
  forall e f g cr rows cols.
  (Matrix e cr U -> Matrix f U cr -> g) ->
  Matrix e cr rows ->
  Matrix f cols cr ->
  Matrix g cols rows
rowsWithColumns f a b = go (colShape b) (rowShape a) a b
  where
    go :: SDim c -> SDim r -> Matrix e cr r -> Matrix f c cr -> Matrix g c r
    go cols rows x y = case splitLongerSide cols rows of
      NoSplit                 -> One (f x y)
      SplitRows top bottom    -> case splitFork x of
        (xTop, xBottom) -> Fork (go cols top    xTop    y)
                                (go cols bottom xBottom y)
      SplitColumns left right -> case splitJoin y of
        (yLeft, yRight) -> Join (go left  rows x yLeft)
                                (go right rows x yRight)
{-# INLINE rowsWithColumns #-}

-- How 'rowsWithColumns' cuts a block of the result: not at all when it is a
-- single element, otherwise into two halves along its longer side, the rows
-- when both sides are equal. Each case tells the type checker which dimension
-- of the block is a sum.
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
{-# INLINE splitLongerSide #-}

{- | A row times a column, as a number: the sum of the products of their
elements, added up as the tree of their shared dimension groups them. These are
the clauses of 'comp' for 1 by 1 matrices and for divide and conquer, at the
element type: @comp r c == One (dot r c)@. A row can only be built from 'Join'
and 'One', and a column from 'Fork' and 'One', so the two clauses cover every
case. At 'Double', GHC compiles 'dot' to a loop that keeps the sum unboxed and
allocates nothing.
-}
dot :: (Num e) => Matrix e cr U -> Matrix e U cr -> e
dot (One x)      (One y)      = x * y
dot (Join x1 x2) (Fork y1 y2) = dot x1 y1 + dot x2 y2
{-# SPECIALISE dot :: Matrix Double cr U -> Matrix Double U cr -> Double #-}
{-# SPECIALISE dot :: Matrix Int cr U -> Matrix Int U cr -> Int #-}
{-# SPECIALISE dot :: Matrix Boolean cr U -> Matrix Boolean U cr -> Boolean #-}

{- | Lays a matrix out row-major, with every 'Fork' above every 'Join', so that
its rows come apart in constant time. The matrix is the same, by the exchange
law (Macedo and Oliveira 2013, eq. 30). It takes time proportional to the size
of the matrix, and to its number of rows when it is already row-major.
-}
rowMajor :: Matrix e cols rows -> Matrix e cols rows
rowMajor m = go (rowShape m) m
  where
    go :: SDim r -> Matrix x c r -> Matrix x c r
    go SU                   x = x
    go (SPlus _ top bottom) x = case splitFork x of
      (xTop, xBottom) -> Fork (go top    xTop)
                              (go bottom xBottom)

{- | Lays a matrix out column-major, with every 'Join' above every 'Fork', so
that its columns come apart in constant time.
-}
columnMajor :: Matrix e cols rows -> Matrix e cols rows
columnMajor m = go (colShape m) m
  where
    go :: SDim c -> Matrix x c r -> Matrix x c r
    go SU                   x = x
    go (SPlus _ left right) x = case splitJoin x of
      (xLeft, xRight) -> Join (go left  xLeft)
                              (go right xRight)

{- | 'comp' on several cores, equal to it bit for bit:

@
result = parComp a b          -- depth chosen from the core count and the size
tuned  = parCompWith 6 a b    -- at most 6 levels of parallel splits
@

The depth grows with the number of capabilities, @ceiling (logBase 2 n) + 2@
levels for @n@ of them, so that there are about four blocks per core for the
scheduler to balance. It is lower for small products, so that no spark gets
fewer than about 2^15 multiply-adds, and 0 (no sparks) on a single
capability. The count is read once, when the program starts; a program that
changes it with 'GHC.Conc.setNumCapabilities' should pass a depth to
'parCompWith'.

Compile with @-threaded@ and run with @+RTS -N@. Products of a few hundred rows
gain from a smaller allocation area, @+RTS -A1m@, with which idle cores pick up
the work sooner; large ones gain a few percent from @-A64m@.
-}
parComp :: forall e cr rows cols. (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
parComp a b = parCompWith (defaultDepth (rows' a) (columns' a) (columns' b)) a b
{-# INLINE parComp #-}

-- The depth 'parComp' uses for an r by k matrix times a k by c one.
defaultDepth :: Int -> Int -> Int -> Int
defaultDepth r k c
  | numCapabilities <= 1 = 0
  | otherwise = max 0 (min (ceilLog2 numCapabilities + 2) (log2 r + log2 k + log2 c - grain))
  where
    grain = 15
    log2 n = finiteBitSize n - 1 - countLeadingZeros n
    ceilLog2 n = log2 (n - 1) + 1

{- | 'comp' with the two halves of each of the first @depth@ splits of the
result computed in parallel, and the first @depth@ levels of laying the
operands out as well: GHC sparks one half and evaluates the other on the
current thread. That makes at most @3 * (2^depth - 1) + 1@ sparks, fewer when
the dimension trees are not that deep. A depth of 0 or less is 'comp'.

Every depth gives the same result as 'comp', bit for bit. Only the result is
split, never the dimension the product sums over, so each element is still the
same 'dot' of the same row and column, and no partial sums are added at the
end. The rewrite rules match 'comp' only, so 'parCompWith' always computes.

A spark evaluates its block to weak head normal form. The fields of the matrix
type are strict, so that evaluates every element of the block to its own weak
head normal form, which for 'Double', 'Int' and t'Boolean' is the whole value.
With an element type whose weak head normal form leaves work undone (a lazy
pair, say), that work happens later, on whichever thread uses the element.

The parallel levels repeat the recursion of 'rowsWithColumns' rather than
sharing it: a single recursion that chose at each level between parallel and
sequential halves would allocate a suspended computation for both halves of
every block, sequential ones included.
-}
parCompWith ::
  forall e cr rows cols.
  (Num e) =>
  Int ->
  Matrix e cr rows ->
  Matrix e cols cr ->
  Matrix e cols rows
parCompWith depth a b
  | depth <= 0 = comp a b
  | otherwise  =
      inParallel
        (go depth (colShape b) (rowShape a))
        (rowMajorPar depth (rowShape a) a)
        (columnMajorPar depth (colShape b) b)
  where
    go :: Int -> SDim c -> SDim r -> Matrix e cr r -> Matrix e c cr -> Matrix e c r
    go d cols rows x y
      | d <= 0 = rowsWithColumns dot x y
      | otherwise = case splitLongerSide cols rows of
          NoSplit                 -> One (dot x y)
          SplitRows top bottom    -> case splitFork x of
            (xTop, xBottom) -> inParallel Fork (go (d - 1) cols top    xTop    y)
                                               (go (d - 1) cols bottom xBottom y)
          SplitColumns left right -> case splitJoin y of
            (yLeft, yRight) -> inParallel Join (go (d - 1) left  rows x yLeft)
                                               (go (d - 1) right rows x yRight)
    rowMajorPar :: Int -> SDim r -> Matrix e c r -> Matrix e c r
    rowMajorPar d (SPlus _ top bottom) m | d > 0 = case splitFork m of
      (mTop, mBottom) -> inParallel Fork (rowMajorPar (d - 1) top    mTop)
                                         (rowMajorPar (d - 1) bottom mBottom)
    rowMajorPar _ _ m = rowMajor m
    columnMajorPar :: Int -> SDim c -> Matrix e c r -> Matrix e c r
    columnMajorPar d (SPlus _ left right) m | d > 0 = case splitJoin m of
      (mLeft, mRight) -> inParallel Join (columnMajorPar (d - 1) left  mLeft)
                                         (columnMajorPar (d - 1) right mRight)
    columnMajorPar _ _ m = columnMajor m
{-# SPECIALISE parCompWith :: Int -> Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}
{-# SPECIALISE parCompWith :: Int -> Matrix Int cr rows -> Matrix Int cols cr -> Matrix Int cols rows #-}
{-# SPECIALISE parCompWith :: Int -> Matrix Boolean cr rows -> Matrix Boolean cols cr -> Matrix Boolean cols rows #-}

-- Applies k to its two arguments after computing them at the same time: the
-- right one in a spark, the left one on this thread.
inParallel :: (x -> y -> z) -> x -> y -> z
inParallel k l r = r `par` (l `pseq` k l r)
{-# INLINE inParallel #-}

{-# RULES
-- Category: identity
"comp/iden-right" forall m. comp m iden = m
"comp/iden-left"  forall m. comp iden m = m
  #-}
