{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
-- The benchmark would otherwise compute the product once and time a shared
-- result.
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}

{- | Measures one configuration of parallel composition per run, with the
runtime system's statistics for each product. docs/parallelism.md has the
tables it produces and the commands that produced them.

@
laop-parallel VARIANT N DEPTH SHAPE REPS +RTS -s -N16
@

* @VARIANT@: @seq@ ('comp'), @par@ ('parCompWith' @DEPTH@), @default@ ('parComp'),
  @old-seq@ and @old-par@ (the algorithm of laop 0.2, which contracts the
  shared dimension first, and the same recursion with sparks, at @DEPTH@).
* @N@: the product of two @N@ by @N@ matrices of 'Double'.
* @SHAPE@: @rows@ (both operands laid out row by row, as 'fromLists' builds
  them), @cols@ (both column by column, as 'tr' of a row-major matrix),
  @mixed@ (the left one column by column), @skew@ (row by row, with every
  dimension split 1:9 at the top).
* @REPS@: how many products to time, after one untimed product that is
  checked against 'comp'.

It prints one line of comma-separated values: variant, n, depth, shape,
capabilities, reps, then per product the fastest and median times in
seconds, gigabytes allocated, garbage collections, GC and mutator seconds
(elapsed), CPU seconds, and the peak live data in megabytes, then whether the
checked product equalled 'comp'. With @+RTS -s@ the runtime system adds the
spark counts on standard error.
-}
module Main (main) where

import           Control.DeepSeq      (force)
import           Control.Exception    (evaluate)
import           Control.Monad        (replicateM, unless)
import           Control.Parallel     (par, pseq)
import           Data.List            (sort)
import           Data.Proxy           (Proxy (..))
import           GHC.Clock            (getMonotonicTime)
import           GHC.Conc             (getNumCapabilities)
import           GHC.Stats
import           GHC.TypeLits         (SomeNat (..), someNatVal)
import           LAoP.Matrix.Internal
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import           System.Mem           (performMajorGC)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [variant, n, depth, shape, reps] -> measure variant (read n) (read depth) shape (read reps)
    _ -> do
      hPutStrLn stderr "usage: laop-parallel VARIANT N DEPTH SHAPE REPS [+RTS ...]"
      exitFailure

measure :: String -> Integer -> Int -> String -> Int -> IO ()
measure variant n depth shape reps = case dimension of
  SomeDim sd -> do
    a <- evaluate (force (build sd (shape `notElem` ["cols", "mixed"]) 0))
    b <- evaluate (force (build sd (shape /= "cols") 1))
    let run = case variant of
          "seq"     -> comp
          "par"     -> parCompWith depth
          "default" -> parComp
          "old-seq" -> compOld
          "old-par" -> compOldPar depth
          _         -> error ("unknown variant " ++ variant)
    expected <- evaluate (force (comp a b))
    got <- evaluate (force (run a b))
    unless (got == expected) (hPutStrLn stderr "the product differs from comp")
    performMajorGC
    s0 <- getRTSStats
    times <- replicateM reps $ do
      t0 <- getMonotonicTime
      _ <- evaluate (force (run a b))
      t1 <- getMonotonicTime
      pure (t1 - t0)
    s1 <- getRTSStats
    caps <- getNumCapabilities
    let per :: (Real x) => (RTSStats -> x) -> Double
        per f = realToFrac (f s1 - f s0) / fromIntegral reps
        seconds x = x / 1e9
    putStrLn $
      concatMap
        (++ ",")
        [ variant, show n, show depth, shape, show caps, show reps
        , show (minimum times), show (sort times !! (reps `div` 2))
        , show (per allocated_bytes / 1e9), show (per gcs)
        , show (seconds (per gc_elapsed_ns)), show (seconds (per mutator_elapsed_ns))
        , show (seconds (per cpu_ns))
        , show (fromIntegral (max_live_bytes s1) / 1e6 :: Double)
        ]
        ++ show (got == expected)
  where
    dimension
      | shape == "skew" = case (ofSize (n `div` 10), ofSize (n - n `div` 10)) of
          (SomeDim s, SomeDim t) -> SomeDim (sPlus s t)
      | otherwise = ofSize n

data SomeDim where
  SomeDim :: SDim d -> SomeDim

ofSize :: Integer -> SomeDim
ofSize n = case someNatVal n of
  Just (SomeNat (_ :: Proxy k)) -> SomeDim (sFromNat @k)
  Nothing                       -> error "negative size"

-- A square matrix with elements in [-0.5, 0.5), laid out row by row or column
-- by column.
build :: SDim d -> Bool -> Int -> Matrix Double d d
build sd byRows salt
  | byRows = generateS sd sd element
  | otherwise = tr (generateS sd sd (flip element))
  where
    element c r = fromIntegral (((c + salt) * 7919 + r * 104729) `mod` 1999) / 1999 - 0.5

-- The composition of laop 0.2: contract the shared dimension first, then add
-- the partial products element by element.
compOld :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
compOld (One a) (One b)       = One (a * b)
compOld (Join a b) (Fork c d) = compOld a c .+. compOld b d
compOld (Fork a b) c          = Fork (compOld a c) (compOld b c)
compOld c (Join a b)          = Join (compOld c a) (compOld c b)
{-# SPECIALISE compOld :: Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}

-- 'compOld' with one of the two halves sparked at each of its first @d@
-- levels, which parallelises the shared dimension too.
compOldPar :: (Num e) => Int -> Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
compOldPar d a b | d <= 0 = compOld a b
compOldPar _ (One a) (One b) = One (a * b)
compOldPar d (Join a b) (Fork c e) =
  let l = compOldPar (d - 1) a c
      r = compOldPar (d - 1) b e
   in r `par` l `pseq` (l .+. r)
compOldPar d (Fork a b) c =
  let l = compOldPar (d - 1) a c
      r = compOldPar (d - 1) b c
   in r `par` l `pseq` Fork l r
compOldPar d c (Join a b) =
  let l = compOldPar (d - 1) c a
      r = compOldPar (d - 1) c b
   in r `par` l `pseq` Join l r
{-# SPECIALISE compOldPar :: Int -> Matrix Double cr rows -> Matrix Double cols cr -> Matrix Double cols rows #-}
