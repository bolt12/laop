{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Representation
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The matrix type, its instances, and the functions that read a matrix: its
blocks, its shape, its elements and its size.
-}
module LAoP.Matrix.Internal.Representation (
  Matrix (..),
  splitJoin,
  splitFork,
  colShape,
  rowShape,
  toLists,
  toList,
  abideJF,
  abideFJ,
  columns,
  columns',
  rows,
  rows',
  pretty,
  prettyPrint,
) where

import           Control.DeepSeq          (NFData (..))
import           LAoP.Matrix.Internal.Dim

-- Matrix GADT

{- | A matrix of elements @e@ with the dimension trees @cols@ and @rows@: a
single element, two matrices side by side (the junc @[a|b]@ of Macedo and
Oliveira 2013), or two matrices one above the other (the split @[a/b]@). The
types fix the two dimension trees; how the 'Join' and 'Fork' nodes nest is the
layout of the matrix, which the exchange law changes without changing the
matrix. Every field is strict, so a matrix in weak head normal form has every
element in weak head normal form.
-}
data Matrix e (cols :: Dim) (rows :: Dim) where
  One  :: !e -> Matrix e U U
  Join :: !(Matrix e left rows) -> !(Matrix e right  rows) -> Matrix e (left :+: right) rows
  Fork :: !(Matrix e cols top)  -> !(Matrix e cols bottom) -> Matrix e cols (top :+: bottom)

deriving instance (Show e) => Show (Matrix e cols rows)

instance (NFData e) => NFData (Matrix e cols rows) where
  rnf (One e)    = rnf e
  rnf (Join a b) = rnf a `seq` rnf b
  rnf (Fork a b) = rnf a `seq` rnf b

-- | Element-wise equality, block by block (Macedo and Oliveira 2013, eqs. 33
-- and 34). Linear in the size even when the two matrices are laid out
-- differently, one row-major and the other column-major.
instance (Eq e) => Eq (Matrix e cols rows) where
  One a == One b       = a == b
  Join a b == Join c d = a == c && b == d
  Fork a b == Fork c d = a == c && b == d
  Join a b == y        = let (c, d) = splitJoin y in a == c && b == d
  Fork a b == y        = let (c, d) = splitFork y in a == c && b == d

{- | Splits a matrix into its left and right column blocks, whatever its
layout, undoing 'LAoP.Matrix.Internal.join'. By the cancellation laws (Macedo and Oliveira 2013,
eq. 28) the result is

@
splitJoin m == (comp m i1, comp m i2)
@

but no product is computed. On a 'Join' this is O(1). On a 'Fork' each step
applies the junc/split exchange ("abide") law,
@Fork (Join a b) (Join c d) == Join (Fork a c) (Fork b d)@, to one level, so
only the 'Fork' spine above the boundary is rebuilt.

For example, take the matrix

@
1 2
3 4
@

stored row by row, as a 'Fork' of its two rows:

@
Fork (Join (One 1) (One 2))   -- row 1 2
     (Join (One 3) (One 4))   -- row 3 4
@

'splitJoin' returns its first column (1 above 3) and its second (2 above 4),
each rebuilt as a 'Fork' of one entry from each row:

>>> splitJoin (Fork (Join (One 1) (One 2)) (Join (One 3) (One 4)))
(Fork (One 1) (One 3),Fork (One 2) (One 4))
-}
splitJoin :: Matrix e (c1 :+: c2) r -> (Matrix e c1 r, Matrix e c2 r)
splitJoin (Join left right)  = (left, right)
splitJoin (Fork top bottom) =
  let (topLeft, topRight)       = splitJoin top
      (bottomLeft, bottomRight) = splitJoin bottom
   in (Fork topLeft bottomLeft, Fork topRight bottomRight)

{- | The row counterpart of 'splitJoin': it returns the top and bottom blocks,
equal to @(comp p1 m, comp p2 m)@ (Macedo and Oliveira 2013, eq. 29). A 'Fork' splits in O(1), and a
'Join' is taken apart one exchange-law step per level of its 'Join' spine.

For example, the same matrix

@
1 2
3 4
@

stored column by column is a 'Join' of its two columns:

@
Join (Fork (One 1) (One 3))   -- column 1 3
     (Fork (One 2) (One 4))   -- column 2 4
@

'splitFork' returns its first row (1 beside 2) and its second (3 beside 4),
each rebuilt as a 'Join':

>>> splitFork (Join (Fork (One 1) (One 3)) (Fork (One 2) (One 4)))
(Join (One 1) (One 2),Join (One 3) (One 4))
-}
splitFork :: Matrix e c (r1 :+: r2) -> (Matrix e c r1, Matrix e c r2)
splitFork (Fork top bottom)  = (top, bottom)
splitFork (Join left right) =
  let (leftTop, leftBottom)   = splitFork left
      (rightTop, rightBottom) = splitFork right
   in (Join leftTop rightTop, Join leftBottom rightBottom)

{- | Lexicographic order on the elements in row-major order. It is a total
order consistent with '=='. For element-wise inclusion of relations use
@LAoP.Relation.sse@.
-}
instance (Ord e) => Ord (Matrix e cols rows) where
  compare a b = compare (toList a) (toList b)

-- Layout

-- | Column tree of a matrix, read off its spine in O(columns).
colShape :: Matrix e cols rows -> SDim cols
colShape (One _)    = SU
colShape (Join l r) = sPlus (colShape l) (colShape r)
colShape (Fork t _) = colShape t

-- | Row tree of a matrix, read off its spine in O(rows).
rowShape :: Matrix e cols rows -> SDim rows
rowShape (One _)    = SU
rowShape (Fork t b) = sPlus (rowShape t) (rowShape b)
rowShape (Join l _) = rowShape l

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Matrix e cols rows -> [[e]]
toLists m = map ($ []) (rowsDL m [])
  where
    -- Rows as difference lists, so joining columns costs O(1) per row, put in
    -- front of the rows that follow, so stacking costs nothing. Linear in the
    -- size of the matrix, whatever its layout.
    rowsDL :: Matrix e c r -> [[e] -> [e]] -> [[e] -> [e]]
    rowsDL (One e) k    = (e :) : k
    rowsDL (Fork t b) k = rowsDL t (rowsDL b k)
    rowsDL (Join l r) k = zipWith (.) (rowsDL l []) (rowsDL r []) ++ k

-- | Converts a matrix to a list of elements.
toList :: Matrix e cols rows -> [e]
toList = concat . toLists

-- Abide laws

{- | Rewrites a matrix by the junc/split exchange ("abide") law of Macedo and
Oliveira (2013, eq. 30), turning each 'Join' of two 'Fork's into a 'Fork' of
two 'Join's:

@
Join (Fork a c) (Fork b d) == Fork (Join a b) (Join c d)
@

Only the layout changes; every element keeps its value and position.
-}
abideJF :: Matrix e cols rows -> Matrix e cols rows
abideJF (Join (Fork a c) (Fork b d)) = Fork (Join (abideJF a) (abideJF b)) (Join (abideJF c) (abideJF d))
abideJF (One e)    = One e
abideJF (Join a b) = Join (abideJF a) (abideJF b)
abideJF (Fork a b) = Fork (abideJF a) (abideJF b)

{- | The exchange law of 'abideJF' read the other way, from a 'Fork' of two
'Join's to a 'Join' of two 'Fork's:

@
Fork (Join a b) (Join c d) == Join (Fork a c) (Fork b d)
@
-}
abideFJ :: Matrix e cols rows -> Matrix e cols rows
abideFJ (Fork (Join a b) (Join c d)) = Join (Fork (abideFJ a) (abideFJ c)) (Fork (abideFJ b) (abideFJ d))
abideFJ (One e)    = One e
abideFJ (Join a b) = Join (abideFJ a) (abideFJ b)
abideFJ (Fork a b) = Fork (abideFJ a) (abideFJ b)

-- Dimensions

{- | Obtain the number of columns.

  The 'KnownDim' constraint provides the dimension in constant time.
For a version without the constraint see 'columns''.
-}
columns :: forall e cols rows. (KnownDim cols) => Matrix e cols rows -> Int
columns _ = dimVal @cols

{- | Obtain the number of columns by traversing the matrix structure.

For a more efficient version see 'columns'.
-}
columns' :: Matrix e cols rows -> Int
columns' (One _)        = 1
columns' (Join lhs rhs) = columns' lhs + columns' rhs
columns' (Fork top _)   = columns' top

{- | Obtain the number of rows.

  The 'KnownDim' constraint provides the dimension in constant time.
For a version without the constraint see 'rows''.
-}
rows :: forall e cols rows. (KnownDim rows) => Matrix e cols rows -> Int
rows _ = dimVal @rows

{- | Obtain the number of rows by traversing the matrix structure.

For a more efficient version see 'rows'.
-}
rows' :: Matrix e cols rows -> Int
rows' (One _)           = 1
rows' (Join lhs _)      = rows' lhs
rows' (Fork top bottom) = rows' top + rows' bottom

-- Pretty printing

-- | Matrix pretty printer
pretty :: (Show e) => Matrix e cols rows -> String
pretty m =
  concat
    [ "\9484 "
    , frame
    , " \9488\n"
    , unlines ["\9474 " ++ unwords (map fill r) ++ " \9474" | r <- cells]
    , "\9492 "
    , frame
    , " \9496"
    ]
  where
    cells = map (map show) (toLists m)
    widest = maximum (map length (concat cells))
    fill str = replicate (widest - length str) ' ' ++ str
    frame = unwords (replicate (columns' m) (fill ""))

-- | Matrix pretty printer
prettyPrint :: (Show e) => Matrix e cols rows -> IO ()
prettyPrint = putStrLn . pretty
