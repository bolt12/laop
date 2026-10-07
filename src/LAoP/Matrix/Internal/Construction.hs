{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Construction
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Matrices from their primitives, from index functions and from lists, the
constant matrices, and transposition.
-}
module LAoP.Matrix.Internal.Construction (
  one,
  join,
  fork,
  (|||),
  (===),
  generate,
  generateS,
  relayout,
  iden,
  zeros,
  ones,
  constant,
  bang,
  col,
  row,
  fromLists,
  tr,
) where

import           Data.Array                          (listArray, (!))
import           Data.Bool                           (bool)
import           Data.Type.Equality                  ((:~:) (..))
import           GHC.Stack                           (HasCallStack)
import           LAoP.Matrix.Internal.Dim
import           LAoP.Matrix.Internal.Representation

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
eq. 17). Fork algebra uses "fork" for pairing, which here is
'LAoP.Matrix.Internal.kr'.
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

-- Construction from index functions

{- | Constructs a matrix from an index function @(col -> row -> e)@, both
indices zero-based.
-}
generate :: forall e cols rows. (KnownDim cols, KnownDim rows) => (Int -> Int -> e) -> Matrix e cols rows
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

-- Construction

-- | Identity matrix.
iden :: forall e d. (Num e, KnownDim d) => Matrix e d d
iden = generate (\c r -> bool 0 1 (c == r))

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
fromLists :: forall e cols rows. (HasCallStack, KnownDim cols, KnownDim rows) => [[e]] -> Matrix e cols rows
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

-- Transposition

-- | Matrix transposition.
tr :: Matrix e cols rows -> Matrix e rows cols
tr (One e)    = One e
tr (Join a b) = Fork (tr a) (tr b)
tr (Fork a b) = Join (tr a) (tr b)
