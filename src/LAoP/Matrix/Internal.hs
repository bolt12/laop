{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The Linear Algebra of Programming (LAoP) extends the Algebra of Programming
from relations, which are Boolean matrices, to matrices over any semiring, all
typed as arrows. Functions are the matrices with a single 1 in each column,
relations the Boolean ones, and probabilistic functions those whose columns are
distributions.

__LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
motivation behind the library, the underlying theory, and implementation details.

This module offers many of the combinators mentioned in the work of
Macedo (2012), Oliveira (2012) and Macedo and Oliveira (2013).

This is an Internal module and it is not supposed to be imported.
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
  unsafeSFromNat,
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
  tr,

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

  -- * Pretty printing
  pretty,
  prettyPrint,
) where

import           Control.DeepSeq
import           Data.Array          (listArray, (!))
import           Data.Bool
import           Data.Type.Equality  ((:~:) (..))
import           GHC.Stack           (HasCallStack)
import           LAoP.Matrix.Dim
import           LAoP.Utils.Internal
import           Prelude             hiding (id, (.))

-- Matrix GADT

-- | LAoP (Linear Algebra of Programming) Inductive Matrix definition.
data Matrix e (cols :: Dim) (rows :: Dim) where
  One  :: e -> Matrix e U U
  Join :: Matrix e a rows -> Matrix e b rows -> Matrix e (a :+: b) rows
  Fork :: Matrix e cols a  -> Matrix e cols b  -> Matrix e cols (a :+: b)

deriving instance (Show e) => Show (Matrix e cols rows)

instance (NFData e) => NFData (Matrix e cols rows) where
  rnf (One e)    = rnf e
  rnf (Join a b) = rnf a `seq` rnf b
  rnf (Fork a b) = rnf a `seq` rnf b

-- | Element-wise equality, block by block (Macedo and Oliveira 2013, eqs. 33
-- and 34). Linear in the size even when the two matrices are laid out
-- differently (one 'Join'-first, the other 'Fork'-first).
instance (Eq e) => Eq (Matrix e cols rows) where
  One a == One b       = a == b
  Join a b == Join c d = a == c && b == d
  Fork a b == Fork c d = a == c && b == d
  Join a b == y        = let (c, d) = splitJoin y in a == c && b == d
  Fork a b == y        = let (c, d) = splitFork y in a == c && b == d

{- | Splits a matrix into its left and right column blocks, whatever its
layout, undoing 'join'. By the cancellation laws (Macedo and Oliveira 2013,
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
splitJoin (Join x y) = (x, y)
splitJoin (Fork t u) =
  let (t1, t2) = splitJoin t
      (u1, u2) = splitJoin u
   in (Fork t1 u1, Fork t2 u2)

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
splitFork (Fork x y) = (x, y)
splitFork (Join l r) =
  let (l1, l2) = splitFork l
      (r1, r2) = splitFork r
   in (Join l1 r1, Join l2 r2)

{- | Zero matrix taking its rows from the first argument and its columns from
the second. Needs no dimension constraints.
-}
zerosFrom :: (Num e) => Matrix e x r -> Matrix e c y -> Matrix e c r
zerosFrom (Fork a1 a2) b = Fork (zerosFrom a1 b) (zerosFrom a2 b)
zerosFrom (Join a1 _) b  = zerosFrom a1 b
zerosFrom (One _) b      = zerosRow b
  where
    zerosRow :: (Num e) => Matrix e c y -> Matrix e c U
    zerosRow (Join b1 b2) = Join (zerosRow b1) (zerosRow b2)
    zerosRow (Fork b1 _)  = zerosRow b1
    zerosRow (One _)      = One 0

{- | Lexicographic order on the elements in row-major order. It is a total
order consistent with '=='. For element-wise inclusion of relations use
@LAoP.Relation.sse@.
-}
instance (Ord e) => Ord (Matrix e cols rows) where
  compare a b = compare (toList a) (toList b)

-- Construction from index functions

{- | Constructs a matrix from an index function @(col -> row -> e)@, both
indices zero-based.
-}
generate :: forall cols rows e. (KnownDim cols, KnownDim rows) => (Int -> Int -> e) -> Matrix e cols rows
generate = generateS (dimSing @cols) (dimSing @rows)

{- | 'generate' with explicit dimension witnesses. Rows are split first
('Fork'), then each single row is split into columns ('Join'). Offsets are
threaded down the recursion, so every cell costs O(1) beyond its node.
-}
generateS :: forall e cols rows. SDim cols -> SDim rows -> (Int -> Int -> e) -> Matrix e cols rows
generateS sc sr f = goR sr 0
  where
    goR :: forall r. SDim r -> Int -> Matrix e cols r
    goR SU !ro            = goC sc 0 ro
    goR (SPlus _ a b) !ro = Fork (goR a ro) (goR b (ro + sizeOf a))
    goC :: forall c. SDim c -> Int -> Int -> Matrix e c U
    goC SU !co !ro            = One (f co ro)
    goC (SPlus _ a b) !co !ro = Join (goC a co ro) (goC b (co + sizeOf a) ro)

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

{- | Re-expresses a matrix over other dimension trees of the same sizes,
keeping every element at its (row, column) position.

When the matrix already has that layout it is returned as is, after comparing
the trees in O(size of the trees). Otherwise it is rebuilt by position in
O(size of the matrix). Throws a runtime error if the sizes differ.
-}
relayout :: (HasCallStack) => SDim cols -> SDim rows -> Matrix e c r -> Matrix e cols rows
relayout sc sr m =
  case (eqSDim mc sc, eqSDim mr sr) of
    (Just Refl, Just Refl) -> m
    _
      | sizeOf mc /= nc || sizeOf mr /= nr ->
          error $
            "relayout: cannot lay a "
              ++ show (sizeOf mr)
              ++ "x"
              ++ show (sizeOf mc)
              ++ " matrix out as "
              ++ show nr
              ++ "x"
              ++ show nc
      | otherwise -> generateS sc sr (\c r -> cells ! (r * nc + c))
  where
    mc = colShape m
    mr = rowShape m
    nc = sizeOf sc
    nr = sizeOf sr
    cells = listArray (0, nr * nc - 1) (toList m)

-- Primitives

-- | Unit matrix constructor
one :: e -> Matrix e U U
one = One

{- | Places two matrices side by side: the junc @[a|b]@ of Macedo and Oliveira
(2013, eq. 16).
-}
join :: Matrix e a rows -> Matrix e b rows -> Matrix e (a :+: b) rows
join = Join

{- | Stacks @a@ above @b@: the split @[a/b]@ of Macedo and Oliveira (2013,
eq. 17). Fork algebra uses "fork" for pairing, which here is 'kr'.
-}
fork :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (a :+: b)
fork = Fork

infixl 3 |||

-- | Matrix @Join@ constructor. An alias of 'join'.
(|||) :: Matrix e a rows -> Matrix e b rows -> Matrix e (a :+: b) rows
(|||) = join

infixl 2 ===

-- | Matrix @Fork@ constructor. An alias of 'fork'.
(===) :: Matrix e cols a -> Matrix e cols b -> Matrix e cols (a :+: b)
(===) = fork

-- Element-wise operations

infixl 6 .+.

{- | Element-wise addition of matrices. It works block by block, as the other
two abide laws of Macedo and Oliveira (2013, eqs. 31 and 32) state:

@
join a b .+. join c d == join (a .+. c) (b .+. d)
fork a b .+. fork c d == fork (a .+. c) (b .+. d)
@
-}
(.+.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.+.) = zipWithM (+)

infixl 6 .-.

-- | Element-wise subtraction of matrices.
(.-.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.-.) = zipWithM (-)

infixl 7 .*.

-- | Element-wise multiplication of matrices (Hadamard product).
(.*.) :: (Num e) => Matrix e cols rows -> Matrix e cols rows -> Matrix e cols rows
(.*.) = zipWithM (*)

{- | Zip two matrices with a given binary function. When the two are laid out
differently, 'splitJoin' and 'splitFork' line the blocks up as the recursion
goes, so the cost stays linear in the size.
-}
zipWithM :: (e -> f -> g) -> Matrix e cols rows -> Matrix f cols rows -> Matrix g cols rows
zipWithM f (One a) (One b)       = One (f a b)
zipWithM f (Join a b) (Join c d) = Join (zipWithM f a c) (zipWithM f b d)
zipWithM f (Fork a b) (Fork c d) = Fork (zipWithM f a c) (zipWithM f b d)
zipWithM f (Join a b) y          = let (c, d) = splitJoin y in Join (zipWithM f a c) (zipWithM f b d)
zipWithM f (Fork a b) y          = let (c, d) = splitFork y in Fork (zipWithM f a c) (zipWithM f b d)

-- | Applies a function to every element.
emap :: (e -> f) -> Matrix e cols rows -> Matrix f cols rows
emap f (One a)    = One (f a)
emap f (Join a b) = Join (emap f a) (emap f b)
emap f (Fork a b) = Fork (emap f a) (emap f b)

-- Scalar operations

infixl 7 .|

-- | Scalar multiplication of matrices.
{-# SPECIALISE (.|) :: Double -> Matrix Double cols rows -> Matrix Double cols rows #-}
(.|) :: (Num e) => e -> Matrix e cols rows -> Matrix e cols rows
(.|) s (One a)    = One (s * a)
(.|) s (Join a b) = Join (s .| a) (s .| b)
(.|) s (Fork a b) = Fork (s .| a) (s .| b)

infixl 7 ./

-- | Scalar division of matrices.
(./) :: (Fractional e) => Matrix e cols rows -> e -> Matrix e cols rows
(./) (One a) s    = One (a / s)
(./) (Join a b) s = Join (a ./ s) (b ./ s)
(./) (Fork a b) s = Fork (a ./ s) (b ./ s)

-- Construction

-- | Identity matrix.
iden :: forall e d. (Num e, KnownDim d) => Matrix e d d
iden = generate (\c r -> bool 0 1 (c == r))
{-# NOINLINE [1] iden #-}

-- | The zero matrix. A matrix wholly filled with zeros.
zeros :: (Num e, KnownDim cols, KnownDim rows) => Matrix e cols rows
zeros = generate (\_ _ -> 0)

{- | The ones matrix. A matrix wholly filled with ones.

  Also known as the T (Top) matrix.
-}
ones :: (Num e, KnownDim cols, KnownDim rows) => Matrix e cols rows
ones = generate (\_ _ -> 1)

-- | The constant matrix constructor. A matrix wholly filled with a given value.
constant :: (KnownDim cols, KnownDim rows) => e -> Matrix e cols rows
constant e = generate (\_ _ -> e)

-- | The T (Top) row vector matrix.
bang :: (Num e, KnownDim cols) => Matrix e cols U
bang = ones

{- | Constructs a column vector matrix. Throws a runtime error unless the list
has exactly one element per row.
-}
col :: (HasCallStack, KnownDim rows) => [e] -> Matrix e U rows
col = fromLists . map pure

{- | Constructs a row vector matrix. Throws a runtime error unless the list has
exactly one element per column.
-}
row :: (HasCallStack, KnownDim cols) => [e] -> Matrix e cols U
row l = fromLists [l]

{- | Build a matrix out of a list of rows. Throws a runtime error unless there
is exactly one list per row and every row has exactly one element per column.
-}
fromLists :: forall cols rows e. (HasCallStack, KnownDim cols, KnownDim rows) => [[e]] -> Matrix e cols rows
fromLists ls
  | length ls /= nr || any ((/= nc) . length) ls =
      error $
        "fromLists: expected "
          ++ show nr
          ++ " rows of "
          ++ show nc
          ++ " elements, got row lengths "
          ++ show (map length ls)
  | otherwise = generateS sc sr (\c r -> cells ! (r * nc + c))
  where
    sc = dimSing @cols
    sr = dimSing @rows
    nc = sizeOf sc
    nr = sizeOf sr
    cells = listArray (0, nr * nc - 1) (concat ls)

-- Conversion

-- | Converts a matrix to a list of lists of elements.
toLists :: Matrix e cols rows -> [[e]]
toLists m = map ($ []) (rowsDL m)
  where
    -- Rows as difference lists, so joining columns costs O(1) per row.
    rowsDL :: Matrix e c r -> [[e] -> [e]]
    rowsDL (One e)    = [(e :)]
    rowsDL (Fork t b) = rowsDL t ++ rowsDL b
    rowsDL (Join l r) = zipWith (.) (rowsDL l) (rowsDL r)

-- | Converts a matrix to a list of elements.
toList :: Matrix e cols rows -> [e]
toList = concat . toLists

-- Transposition

-- | Matrix transposition.
tr :: Matrix e cols rows -> Matrix e rows cols
tr (One e)    = One e
tr (Join a b) = Fork (tr a) (tr b)
tr (Fork a b) = Join (tr a) (tr b)

-- Composition

{- | Matrix composition. Equivalent to matrix-matrix multiplication.

  This definition takes advantage of divide-and-conquer and fusion laws
from LAoP.

Optimised builds rewrite @comp m iden@ and @comp iden m@ to @m@ using the RULES
in this module. The rules assume exact arithmetic: if a matrix holds @NaN@ or
an infinity, the result can differ from an unoptimised build, because the
skipped products by zero would have propagated it.
-}
comp :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
comp (One a) (One b)       = One (a * b)
comp (Join a b) (Fork c d) = comp a c .+. comp b d
comp (Fork a b) c          = Fork (comp a c) (comp b c)
comp c (Join a b)          = Join (comp c a) (comp c b)
{-# NOINLINE [1] comp #-}
-- Phase [0], after the rewrite rules on 'comp' have had their chance.
{-# SPECIALISE [0] comp :: Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}

{-# RULES
"comp/iden-right" forall m. comp m iden = m
"comp/iden-left"  forall m. comp iden m = m
  #-}

-- Projections

{- | First projection of the standard biproduct, @[id|0]@ (Macedo and Oliveira
2013, eq. 23). The Khatri-Rao projections are 'fstM' and 'sndM'.
-}
p1 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e (m :+: n) m
p1 = Join iden zeros

-- | Second biproduct projection, @[0|id]@.
p2 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e (m :+: n) n
p2 = Join zeros iden

-- Injections

-- | First biproduct injection, @[id/0]@, the transpose of 'p1'.
i1 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e m (m :+: n)
i1 = tr p1

-- | Second biproduct injection, @[0/id]@.
i2 :: forall e m n. (Num e, KnownDim m, KnownDim n) => Matrix e n (m :+: n)
i2 = tr p2

-- Direct sum

infixl 5 -|-

{- | Direct sum: the block-diagonal matrix with @a@ top left, @b@ bottom right
and zeros elsewhere. It is the bifunctor of the biproduct, so both the
coproduct and the product functor of the category of matrices (Macedo and
Oliveira 2013, eq. 62):

@
a -|- b == join (i1 . a) (i2 . b)
@
-}
(-|-) ::
  (Num e) =>
  Matrix e n k ->
  Matrix e m j ->
  Matrix e (n :+: m) (k :+: j)
(-|-) a b = Join (Fork a (zerosFrom b a)) (Fork (zerosFrom a b) b)

-- Khatri-Rao product projections

{- | First projection of the Khatri-Rao product, taking the pair @(i, j)@ to
@i@. Macedo and Oliveira (2013, sec. 13) call it @p1@ and define it as
@iden >< bang@.
-}
fstM ::
  forall e m k.
  (Num e, KnownDim m, KnownDim k) =>
  Matrix e (DimProd m k) m
fstM =
  let sm = dimSing @m
      sk = dimSing @k
   in generateS (sDimProd sm sk) sm (\c r -> if div c (sizeOf sk) == r then 1 else 0)

{- | Second projection of the Khatri-Rao product, taking @(i, j)@ to @j@,
defined as @bang >< iden@.
-}
sndM ::
  forall e m k.
  (Num e, KnownDim m, KnownDim k) =>
  Matrix e (DimProd m k) k
sndM =
  let sm = dimSing @m
      sk = dimSing @k
   in generateS (sDimProd sm sk) sk (\c r -> if mod c (sizeOf sk) == r then 1 else 0)

{- | Khatri Rao Matrix product also known as matrix pairing.

The laws below hold for column-stochastic matrices, that is probabilistic
functions (functions included), where Khatri-Rao is a weak product (Murta and
Oliveira 2013, eq. 22). For other matrices each projection scales its factor by
the column sums of the other.

  NOTE: This is not a true categorical product, see for instance:

@
           | fstM . kr a b == a
kr a b ==> |
           | sndM . kr a b == b
@

__Emphasis__ on the implication symbol.
-}
kr ::
  (Num e) =>
  Matrix e cols a ->
  Matrix e cols b ->
  Matrix e cols (DimProd a b)
kr (One x) b      = x .| b
kr (Fork a1 a2) b = Fork (kr a1 b) (kr a2 b)
kr (Join a1 a2) b = let (b1, b2) = splitJoin b in Join (kr a1 b1) (kr a2 b2)

-- Kronecker product

infixl 4 ><

{- | Kronecker product: every element @x@ of @a@ becomes the block @x .| b@.
It is the tensor product of the category of matrices; the categorical product
there is the biproduct (see @-|-@). Macedo and Oliveira (2013, sec. 13) define
it from the Khatri-Rao product,

@
a >< b == kr (a . fstM) (b . sndM)
@

and this module computes it block by block, following their Kronecker fusion
laws (eqs. 60 and 61):

@
join a b >\< c == join (a >\< c) (b >\< c)
fork a b >\< c == fork (a >\< c) (b >\< c)
@

Macedo and Oliveira let products bind tighter than sums, but @><@ is
@infixl 4@: @a >< b .+. c@ parses as @a >< (b .+. c)@, and @a >< b == c@ needs
parentheses.
-}
(><) ::
  (Num e) =>
  Matrix e m p ->
  Matrix e n q ->
  Matrix e (DimProd m n) (DimProd p q)
(><) (One x) b      = x .| b
(><) (Join a1 a2) b = Join (a1 >< b) (a2 >< b)
(><) (Fork a1 a2) b = Fork (a1 >< b) (a2 >< b)

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

-- Selective

{- | Selective functors 'select' operator equivalent inspired by the
ArrowMonad instance in Selective Applicative Functors (Mokhov et al. 2019).
-}
select ::
  (Num e) =>
  Matrix e cols (a :+: b) ->
  Matrix e a b ->
  Matrix e cols b
select (Fork a b) y   = comp y a .+. b
select (Join m1 m2) y = Join (select m1 y) (select m2 y)

{- | Selective functors 'branch' operator: the left component of the input is
sent through the first matrix, the right component through the second.
-}
branch ::
  (Num e) =>
  Matrix e cols (a :+: b) ->
  Matrix e a c ->
  Matrix e b c ->
  Matrix e cols c
branch x l r =
  let (xa, xb) = splitFork x
   in Fork xa (Fork xb (zerosFrom l x)) `select` Fork (zerosFrom xb l) l `select` r

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
