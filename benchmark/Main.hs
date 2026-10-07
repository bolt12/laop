{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- Arbitrary instances for library types live here, not in the library, so the
-- library does not depend on QuickCheck.
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Criterion.Main
import           Criterion.Types       (Config (..))
import qualified Data.Matrix           as DM
import           Data.Proxy
import qualified Data.Vector           as V
import           GHC.TypeLits
import           LAoP.Category
import           LAoP.Dist.Internal    hiding (choose)
import           LAoP.Index
import           LAoP.Matrix.Indexed   hiding (select)
import qualified LAoP.Relation         as R
import qualified Linear.Matrix         as LM
import qualified Linear.V              as LV
import qualified Numeric.LinearAlgebra as H
import           Prelude               hiding (id, (.))
import qualified Prelude
import           Test.QuickCheck       hiding ((><))

-- Generators

randomMatrix ::
  forall a b.
  (MatIndex a, MatIndex b) =>
  Gen (Matrix Double a b)
randomMatrix = do
  let c = cardinality @a
      r = cardinality @b
  l <- vectorOf (c * r) arbitrary
  return (fromLists (buildList l c))

randomRelation ::
  forall a b.
  (MatIndex a, MatIndex b) =>
  Gen (R.Relation a b)
randomRelation = do
  let c = cardinality @a
      r = cardinality @b
  l <- vectorOf (c * r) (elements [0, 1])
  return (R.fromLists (buildList l c))

randomDist ::
  forall a. (MatIndex a) =>
  Gen (Dist a)
randomDist = do
  let size = cardinality @a
  l <- vectorOf size (abs <$> (arbitrary :: Gen Prob))
  let s = sum l
  return (D (fromLists (map (\x -> [x / s]) l)))

buildList :: [a] -> Int -> [[a]]
buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

instance CoArbitrary (Ranged a b) where
  coarbitrary (Rng i) = coarbitrary i

instance forall (a :: Nat) (b :: Nat). (KnownNat a, KnownNat b) => Arbitrary (Ranged a b) where
  arbitrary =
    let bottom = fromInteger (natVal (Proxy :: Proxy a))
        top = fromInteger (natVal (Proxy :: Proxy b))
     in mkRanged <$> choose (bottom, top)

-- Size aliases

type S5    = Ranged 0 4
type S10   = Ranged 0 9
type S16   = Ranged 0 15
type S20   = Ranged 0 19
type S30   = Ranged 0 29
type S50   = Ranged 0 49
type S100  = Ranged 0 99
type S200  = Ranged 0 199
type S500  = Ranged 0 499
type S1000 = Ranged 0 999

genM :: forall a b. (MatIndex a, MatIndex b) => IO (Matrix Double a b)
genM = generate (resize 1 (randomMatrix @a @b))

genR :: forall a b. (MatIndex a, MatIndex b) => IO (R.Relation a b)
genR = generate (resize 1 (randomRelation @a @b))

genD :: forall a. (MatIndex a) => IO (Dist a)
genD = generate (resize 1 (randomDist @a))

genLists :: Int -> IO [[Double]]
genLists n = buildList <$> generate (vectorOf (n * n) (arbitrary :: Gen Double)) <*> pure n

-- Helpers

-- An identity-shaped matrix whose diagonal value comes from the argument.
identity :: forall a. (MatIndex a) => Double -> Matrix Double a a
identity x = matrixBuilder' (\(r, c) -> if r == c then x else 0)

selectM2 ::
  (Num e, MatIndex b) =>
  Matrix e cols (Either a b) -> Matrix e a b -> Matrix e cols b
selectM2 m y = join y iden `comp` m

-- noRule wrappers: NOINLINE prevents GHC from matching rewrite rules. They are
-- monomorphic, so the call inside reaches the Double specialisation, as a
-- direct call does.
-- Used for the "no fusion" baseline in the fusion pipeline benchmark.

noRuleComp :: Matrix Double cr b -> Matrix Double a cr -> Matrix Double a b
noRuleComp = comp
{-# NOINLINE noRuleComp #-}

noRuleFork :: Matrix e a b -> Matrix e a c -> Matrix e a (Either b c)
noRuleFork = fork
{-# NOINLINE noRuleFork #-}

noRuleJoin :: Matrix e a c -> Matrix e b c -> Matrix e (Either a b) c
noRuleJoin = join
{-# NOINLINE noRuleJoin #-}

noRuleP1 ::
  forall e a.
  (Num e, MatIndex a) =>
  Matrix e (Either a a) a
noRuleP1 = p1
{-# NOINLINE noRuleP1 #-}

noRuleP2 ::
  forall e a.
  (Num e, MatIndex a) =>
  Matrix e (Either a a) a
noRuleP2 = p2
{-# NOINLINE noRuleP2 #-}

noRuleI1 ::
  forall e a.
  (Num e, MatIndex a) =>
  Matrix e a (Either a a)
noRuleI1 = i1
{-# NOINLINE noRuleI1 #-}

noRuleTr :: Matrix e a b -> Matrix e b a
noRuleTr = tr
{-# NOINLINE noRuleTr #-}

noRuleAdd :: Matrix Double a b -> Matrix Double a b -> Matrix Double a b
noRuleAdd = (.+.)
{-# NOINLINE noRuleAdd #-}

noRuleHad :: Matrix Double a b -> Matrix Double a b -> Matrix Double a b
noRuleHad = (.*.)
{-# NOINLINE noRuleHad #-}

-- Fusion pipeline: eight biproduct steps, each matching one rewrite rule.
-- With the rules, the entire pipeline reduces to `a` (zero multiplies).

fusionPipeline ::
  forall a.
  (MatIndex a) =>
  Matrix Double a a -> Matrix Double a a -> Matrix Double a a
fusionPipeline a b =
  let s1 = comp p1 (fork a b)
      s2 = comp s1 iden
      s3 = tr (tr s2)
      s4 = s3 .+. zeros
      s5 = s4 .*. ones
      s6 = comp (join s5 b) i1
      s7 = comp iden s6
      s8 = comp p2 (fork b s7)
  in s8

-- Same pipeline with noRule wrappers: no fusion, all computations run.

noFusionPipeline ::
  forall a.
  (MatIndex a) =>
  Matrix Double a a -> Matrix Double a a -> Matrix Double a a
noFusionPipeline a b =
  let s1 = noRuleComp (noRuleP1 @Double @a) (noRuleFork a b)
      s2 = noRuleComp s1 iden
      s3 = noRuleTr (noRuleTr s2)
      s4 = noRuleAdd s3 zeros
      s5 = noRuleHad s4 ones
      s6 = noRuleComp (noRuleJoin s5 b) (noRuleI1 @Double @a)
      s7 = noRuleComp iden s6
      s8 = noRuleComp (noRuleP2 @Double @a) (noRuleFork b s7)
  in s8

-- The LAoP pipeline written step for step in hmatrix: the same products with
-- the same projection and injection matrices, so both sides do the same work.
hmatrixPipeline :: H.Matrix Double -> H.Matrix Double -> Int -> H.Matrix Double
hmatrixPipeline ha hb n =
  let i   = H.ident n
      z   = H.konst 0 (n, n)
      o   = H.konst 1 (n, n)
      p1' = H.fromBlocks [[i, z]]
      p2' = H.fromBlocks [[z, i]]
      i1' = H.fromBlocks [[i], [z]]
      s1  = p1' H.<> H.fromBlocks [[ha], [hb]]
      s2  = s1 H.<> i
      s3  = H.tr (H.tr s2)
      s4  = s3 + z
      s5  = s4 * o
      s6  = H.fromBlocks [[s5, hb]] H.<> i1'
      s7  = i H.<> s6
      s8  = p2' H.<> H.fromBlocks [[hb], [s7]]
  in s8

-- Cross-library conversion

toHMatrix :: Matrix Double a b -> H.Matrix Double
toHMatrix m =
  let ls = toLists m
      r = length ls
      c = case ls of
        []    -> 0
        x : _ -> length x
  in (r H.>< c) (concat ls)

toDMatrix :: Matrix Double a b -> DM.Matrix Double
toDMatrix = DM.fromLists . toLists

toLinearM :: forall n. (KnownNat n) => [[Double]] -> LV.V n (LV.V n Double)
toLinearM ls =
  let mkRow xs = case LV.fromVector (V.fromList xs) of
        Just v  -> v
        Nothing -> error "toLinearM: row size mismatch"
  in case LV.fromVector (V.fromList (map mkRow ls)) of
    Just v  -> v
    Nothing -> error "toLinearM: matrix size mismatch"

-- Main

main :: IO ()
main =
  defaultMainWith defaultConfig { timeLimit = 30 }
    [ -- Construction: fromLists
      bgroup "Construction/fromLists"
        [ env (genLists 10)  $ \l -> bench "10"  $ nf (\xs -> fromLists xs :: Matrix Double S10  S10)  l
        , env (genLists 50)  $ \l -> bench "50"  $ nf (\xs -> fromLists xs :: Matrix Double S50  S50)  l
        , env (genLists 100) $ \l -> bench "100" $ nf (\xs -> fromLists xs :: Matrix Double S100 S100) l
        , env (genLists 200) $ \l -> bench "200" $ nf (\xs -> fromLists xs :: Matrix Double S200 S200) l
        , env (genLists 500) $ \l -> bench "500" $ nf (\xs -> fromLists xs :: Matrix Double S500 S500) l
        ]

      -- Construction of an identity-shaped matrix. It depends on the benchmark
      -- argument, so it is rebuilt every run; `const iden` would be a shared
      -- constant and time only the traversal.
    , bgroup "Construction/identity"
        [ bench "10"  $ nf (\x -> identity x :: Matrix Double S10  S10)  1
        , bench "50"  $ nf (\x -> identity x :: Matrix Double S50  S50)  1
        , bench "100" $ nf (\x -> identity x :: Matrix Double S100 S100) 1
        , bench "200" $ nf (\x -> identity x :: Matrix Double S200 S200) 1
        , bench "500" $ nf (\x -> identity x :: Matrix Double S500 S500) 1
        ]

      -- Composition
    , bgroup "Composition"
        [ env ((,) <$> genM @S10  @S10  <*> genM @S10  @S10)  $ \ ~(a, b) -> bench "10x10"   $ nf (comp a) b
        , env ((,) <$> genM @S50  @S50  <*> genM @S50  @S50)  $ \ ~(a, b) -> bench "50x50"   $ nf (comp a) b
        , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) -> bench "100x100" $ nf (comp a) b
        , env ((,) <$> genM @S200 @S200 <*> genM @S200 @S200) $ \ ~(a, b) -> bench "200x200" $ nf (comp a) b
        , env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) -> bench "500x500" $ nf (comp a) b
        , env ((,) <$> genM @S1000 @S1000 <*> genM @S1000 @S1000) $ \ ~(a, b) -> bench "1000x1000" $ nf (comp a) b
        ]

      -- Parallel composition: depth sweep. Run with +RTS -N<k>.
    , bgroup "Parallel/depth-sweep"
        [ env ((,) <$> genM @S50 @S50 <*> genM @S50 @S50) $ \ ~(a, b) ->
            bgroup "50x50"
              [ bench "seq"     $ nf (comp a) b
              , bench "depth=1" $ nf (parCompWith 1 a) b
              , bench "depth=2" $ nf (parCompWith 2 a) b
              , bench "depth=4" $ nf (parCompWith 4 a) b
              ]
        , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) ->
            bgroup "100x100"
              [ bench "seq"     $ nf (comp a) b
              , bench "depth=2" $ nf (parCompWith 2 a) b
              , bench "depth=4" $ nf (parCompWith 4 a) b
              , bench "depth=6" $ nf (parCompWith 6 a) b
              ]
        , env ((,) <$> genM @S200 @S200 <*> genM @S200 @S200) $ \ ~(a, b) ->
            bgroup "200x200"
              [ bench "seq"     $ nf (comp a) b
              , bench "depth=2" $ nf (parCompWith 2 a) b
              , bench "depth=4" $ nf (parCompWith 4 a) b
              , bench "depth=6" $ nf (parCompWith 6 a) b
              , bench "depth=8" $ nf (parCompWith 8 a) b
              ]
        , env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) ->
            bgroup "500x500"
              [ bench "seq"     $ nf (comp a) b
              , bench "depth=2" $ nf (parCompWith 2 a) b
              , bench "depth=4" $ nf (parCompWith 4 a) b
              , bench "depth=6" $ nf (parCompWith 6 a) b
              , bench "depth=8" $ nf (parCompWith 8 a) b
              ]
        , env ((,) <$> genM @S1000 @S1000 <*> genM @S1000 @S1000) $ \ ~(a, b) ->
            bgroup "1000x1000"
              [ bench "seq"     $ nf (comp a) b
              , bench "depth=4" $ nf (parCompWith 4 a) b
              , bench "depth=6" $ nf (parCompWith 6 a) b
              , bench "depth=8" $ nf (parCompWith 8 a) b
              ]
        ]

      -- Parallel vs sequential at the default depth. Run with +RTS -N<k>.
    , bgroup "Parallel/default"
        [ env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) ->
            bgroup "100x100"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        , env ((,) <$> genM @S200 @S200 <*> genM @S200 @S200) $ \ ~(a, b) ->
            bgroup "200x200"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        , env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) ->
            bgroup "500x500"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        , env ((,) <$> genM @S1000 @S1000 <*> genM @S1000 @S1000) $ \ ~(a, b) ->
            bgroup "1000x1000"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        ]

      -- Parallel composition of relations (Boolean matrices, half the
      -- elements set). Run with +RTS -N<k>.
    , bgroup "Parallel/relation"
        [ env ((,) <$> genR @S200 @S200 <*> genR @S200 @S200) $ \ ~(R.R a, R.R b) ->
            bgroup "200x200"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        , env ((,) <$> genR @S500 @S500 <*> genR @S500 @S500) $ \ ~(R.R a, R.R b) ->
            bgroup "500x500"
              [ bench "seq"      $ nf (comp a) b
              , bench "parComp" $ nf (parComp a) b
              ]
        ]

      -- Composition when the operands are not both laid out row by row
      -- (tr turns the row-first layout of fromLists into a column-first one).
    , bgroup "Composition/layout"
        [ env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) ->
            bench "500 rows . rows" $ nf (comp a) b
        , env ((,) <$> (tr <$> genM @S500 @S500) <*> genM @S500 @S500) $ \ ~(a, b) ->
            bench "500 columns . rows" $ nf (comp a) b
        , env ((,) <$> genM @S500 @S500 <*> (tr <$> genM @S500 @S500)) $ \ ~(a, b) ->
            bench "500 rows . columns" $ nf (comp a) b
        ]

      -- Cross-library comparison: matrix multiplication
    , bgroup "CrossLibrary"
        [ env (do a <- genM @S100 @S100; b <- genM @S100 @S100
                  let ha = toHMatrix a; hb = toHMatrix b
                  let da = toDMatrix a; db = toDMatrix b
                  let la = toLinearM @100 (toLists a); lb = toLinearM @100 (toLists b)
                  pure (a, b, ha, hb, da, db, la, lb)) $
            \ ~(a, b, ha, hb, da, db, la, lb) ->
              bgroup "100x100"
                [ bench "LAoP/seq"      $ nf (comp a) b
                , bench "LAoP/par"      $ nf (parComp a) b
                , bench "hmatrix"       $ nf (ha H.<>) hb
                , bench "Data.Matrix"   $ nf (DM.multStd da) db
                , bench "linear"        $ nf (la LM.!*!) lb
                ]
        , env (do a <- genM @S200 @S200; b <- genM @S200 @S200
                  let ha = toHMatrix a; hb = toHMatrix b
                  let da = toDMatrix a; db = toDMatrix b
                  pure (a, b, ha, hb, da, db)) $
            \ ~(a, b, ha, hb, da, db) ->
              bgroup "200x200"
                [ bench "LAoP/seq"      $ nf (comp a) b
                , bench "LAoP/par"      $ nf (parComp a) b
                , bench "hmatrix"       $ nf (ha H.<>) hb
                , bench "Data.Matrix"   $ nf (DM.multStd da) db
                ]
        , env (do a <- genM @S500 @S500; b <- genM @S500 @S500
                  let ha = toHMatrix a; hb = toHMatrix b
                  let da = toDMatrix a; db = toDMatrix b
                  pure (a, b, ha, hb, da, db)) $
            \ ~(a, b, ha, hb, da, db) ->
              bgroup "500x500"
                [ bench "LAoP/seq"      $ nf (comp a) b
                , bench "LAoP/par"      $ nf (parComp a) b
                , bench "hmatrix"       $ nf (ha H.<>) hb
                , bench "Data.Matrix"   $ nf (DM.multStd da) db
                ]
        , env (do a <- genM @S1000 @S1000; b <- genM @S1000 @S1000
                  let ha = toHMatrix a; hb = toHMatrix b
                  let da = toDMatrix a; db = toDMatrix b
                  pure (a, b, ha, hb, da, db)) $
            \ ~(a, b, ha, hb, da, db) ->
              bgroup "1000x1000"
                [ bench "LAoP/seq"      $ nf (comp a) b
                , bench "LAoP/par"      $ nf (parComp a) b
                , bench "hmatrix"       $ nf (ha H.<>) hb
                , bench "Data.Matrix"   $ nf (DM.multStd da) db
                ]
        ]

      -- Transpose
    , bgroup "Transpose"
        [ env (genM @S10  @S10)  $ \m -> bench "10"  $ nf tr m
        , env (genM @S100 @S100) $ \m -> bench "100" $ nf tr m
        , env (genM @S500 @S500) $ \m -> bench "500" $ nf tr m
        ]

      -- Element-wise
    , bgroup "Element-wise/addition"
        [ env ((,) <$> genM @S10  @S10  <*> genM @S10  @S10)  $ \ ~(a, b) -> bench "10"  $ nf (a .+.) b
        , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) -> bench "100" $ nf (a .+.) b
        , env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) -> bench "500" $ nf (a .+.) b
        ]
    , bgroup "Element-wise/Hadamard"
        [ env ((,) <$> genM @S10  @S10  <*> genM @S10  @S10)  $ \ ~(a, b) -> bench "10"  $ nf (a .*.) b
        , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) -> bench "100" $ nf (a .*.) b
        , env ((,) <$> genM @S500 @S500 <*> genM @S500 @S500) $ \ ~(a, b) -> bench "500" $ nf (a .*.) b
        ]
    , bgroup "Element-wise/scalar"
        [ env (genM @S10  @S10)  $ \m -> bench "10"  $ nf (3.14 .|) m
        , env (genM @S100 @S100) $ \m -> bench "100" $ nf (3.14 .|) m
        , env (genM @S500 @S500) $ \m -> bench "500" $ nf (3.14 .|) m
        ]

      -- Kronecker product of two n x n matrices: n^2 x n^2 result
    , bgroup "Kronecker"
        [ env ((,) <$> genM @S5 @S5 <*> genM @S5 @S5)     $ \ ~(a, b) -> bench "5"  $ nf (a ><) b
        , env ((,) <$> genM @S10 @S10 <*> genM @S10 @S10) $ \ ~(a, b) -> bench "10" $ nf (a ><) b
        , env ((,) <$> genM @S16 @S16 <*> genM @S16 @S16) $ \ ~(a, b) -> bench "16" $ nf (a ><) b
        ]

      -- Khatri-Rao product of two n x n matrices: n columns, n^2 rows
    , bgroup "KhatriRao"
        [ env ((,) <$> genM @S10 @S10 <*> genM @S10 @S10) $ \ ~(a, b) -> bench "10" $ nf (kr a) b
        , env ((,) <$> genM @S20 @S20 <*> genM @S20 @S20) $ \ ~(a, b) -> bench "20" $ nf (kr a) b
        , env ((,) <$> genM @S30 @S30 <*> genM @S30 @S30) $ \ ~(a, b) -> bench "30" $ nf (kr a) b
        ]

      -- Direct sum of two n x n matrices
    , bgroup "DirectSum"
        [ env ((,) <$> genM @S50 @S50 <*> genM @S50 @S50)     $ \ ~(a, b) -> bench "50"  $ nf (a -|-) b
        , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) -> bench "100" $ nf (a -|-) b
        ]

      -- Element-wise addition when one operand is laid out columns-first
      -- (tr turns the row-first layout of fromLists into a column-first one).
    , bgroup "Element-wise/addition-mixed-layout"
        [ env ((,) <$> genM @S100 @S100 <*> (tr <$> genM @S100 @S100)) $ \ ~(a, b) -> bench "100" $ nf (a .+.) b
        , env ((,) <$> genM @S500 @S500 <*> (tr <$> genM @S500 @S500)) $ \ ~(a, b) -> bench "500" $ nf (a .+.) b
        ]

      -- Identity RULES
    , bgroup "Identity RULES"
        [ env (genM @S100 @S100) $ \m ->
            bgroup "100"
              [ bench "comp m iden (rule fires)" $ nf (`comp` (iden :: Matrix Double S100 S100)) m
              , bench "comp m iden (no rule)"    $ nf (`noRuleComp` (iden :: Matrix Double S100 S100)) m
              ]
        , env (genM @S500 @S500) $ \m ->
            bgroup "500"
              [ bench "comp m iden (rule fires)" $ nf (`comp` (iden :: Matrix Double S500 S500)) m
              , bench "comp m iden (no rule)"    $ nf (`noRuleComp` (iden :: Matrix Double S500 S500)) m
              ]
        ]

      -- Select
    , env ((,) <$> genM @S100 @(Either S100 S100) <*> genM @S100 @S100) $ \ ~(m, y) ->
        bgroup "Select (100+100)"
          [ bench "selectM (selective)"    $ nf (selectM m) y
          , bench "selectM2 (applicative)" $ nf (selectM2 m) y
          ]

      -- Relational
    , bgroup "Relational"
        [ env ((,) <$> genR @S50  @S50  <*> genR @S50  @S50)  $ \ ~(a, b) ->
            bgroup "50"
              [ bench "comp"         $ nf (R.comp a) b
              , bench "conv"         $ nf R.conv a
              , bench "intersection" $ nf (R.intersection a) b
              , bench "union"        $ nf (R.union a) b
              ]
        , env ((,) <$> genR @S100 @S100 <*> genR @S100 @S100) $ \ ~(a, b) ->
            bgroup "100"
              [ bench "comp"         $ nf (R.comp a) b
              , bench "conv"         $ nf R.conv a
              , bench "intersection" $ nf (R.intersection a) b
              , bench "union"        $ nf (R.union a) b
              ]
        ]

      -- Distribution
    , bgroup "Distribution"
        [ env (genD @S50) $ \d ->
            bgroup "50"
              [ bench "fmapD id" $ nf (fmapD Prelude.id) d
              , bench "?? True"  $ nf (const True ??) d
              ]
        , env (genD @S100) $ \d ->
            bgroup "100"
              [ bench "fmapD id" $ nf (fmapD Prelude.id) d
              , bench "?? True"  $ nf (const True ??) d
              ]
        ]

      -- Non-square composition
    , env ((,) <$> genM @S50 @S100 <*> genM @S100 @S200) $ \ ~(a, b) ->
        bgroup "Non-square"
          [ bench "comp 100x200 . 50x100" $ nf (comp b) a
          ]

      -- Fusion pipeline: 8 biproduct steps that the rewrite rules reduce to the
      -- first input. LAoP/fusion is that reduced program; LAoP/no-fusion runs
      -- every step. hmatrix runs the same steps with BLAS, and
      -- hmatrix/simplified is the hand-reduced program (just the first input),
      -- the fair baseline for LAoP/fusion.
    , bgroup "FusionPipeline"
        [ env (do a <- genM @S100 @S100; b <- genM @S100 @S100
                  let ha = toHMatrix a; hb = toHMatrix b
                  pure (a, b, ha, hb)) $
            \ ~(a, b, ha, hb) ->
              bgroup "100x100"
                [ bench "LAoP/fusion"    $ nf (fusionPipeline a) b
                , bench "LAoP/no-fusion" $ nf (noFusionPipeline a) b
                , bench "hmatrix"        $ nf (\h -> hmatrixPipeline ha h 100) hb
                , bench "hmatrix/simplified" $ nf (const ha) hb
                ]
        , env (do a <- genM @S200 @S200; b <- genM @S200 @S200
                  let ha = toHMatrix a; hb = toHMatrix b
                  pure (a, b, ha, hb)) $
            \ ~(a, b, ha, hb) ->
              bgroup "200x200"
                [ bench "LAoP/fusion"    $ nf (fusionPipeline a) b
                , bench "LAoP/no-fusion" $ nf (noFusionPipeline a) b
                , bench "hmatrix"        $ nf (\h -> hmatrixPipeline ha h 200) hb
                , bench "hmatrix/simplified" $ nf (const ha) hb
                ]
        , env (do a <- genM @S500 @S500; b <- genM @S500 @S500
                  let ha = toHMatrix a; hb = toHMatrix b
                  pure (a, b, ha, hb)) $
            \ ~(a, b, ha, hb) ->
              bgroup "500x500"
                [ bench "LAoP/fusion"    $ nf (fusionPipeline a) b
                , bench "hmatrix"        $ nf (\h -> hmatrixPipeline ha h 500) hb
                , bench "hmatrix/simplified" $ nf (const ha) hb
                ]
        ]

      -- Biproduct
    , env ((,) <$> genM @S100 @S100 <*> genM @S100 @S100) $ \ ~(a, b) ->
        bgroup "Biproduct/100"
          -- fork and join only allocate one node: whnf times that, not a walk
          -- of the already evaluated operands.
          [ bench "fork" $ whnf (uncurry fork) (a, b)
          , bench "join" $ whnf (uncurry join) (a, b)
          , bench "p1"   $ nf (comp (p1 :: Matrix Double (Either S100 S100) S100)) (fork a b)
          , bench "p2"   $ nf (comp (p2 :: Matrix Double (Either S100 S100) S100)) (fork a b)
          ]
    ]
