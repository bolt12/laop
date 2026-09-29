{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Matrix.Reference (matrixReferenceTests) where

import           Data.List                    (transpose)
import qualified Data.Matrix                  as DM
import           LAoP.Matrix.Indexed
import qualified LAoP.Matrix.Parallel.Indexed as Par
import           LAoP.Utils
import           Prelude                      hiding (id, (.))
import           Test.Generators
import           Test.QuickCheck              hiding ((><))
import           Test.Tasty
import           Test.Tasty.QuickCheck        hiding (forAll, (><))

matrixReferenceTests :: TestTree
matrixReferenceTests =
  testGroup "Matrix reference (vs Data.Matrix)"
    [ testProperty "multiplication" prop_refMult
    , testProperty "transpose" prop_refTranspose
    , testProperty "addition" prop_refAddition
    , testProperty "scalar multiply" prop_refScalar
    , testProperty "identity" prop_refIdentity
    , testProperty "multiplication (Int, exact)" prop_refMultInt
    , testProperty "Kronecker product" prop_refKronecker
    , testProperty "Khatri-Rao product" prop_refKhatriRao
    , testProperty "direct sum" prop_refDirectSum
    , testProperty "select == join y iden . m" prop_refSelect
    , testProperty "branch == l . p1 . x + r . p2 . x" prop_refBranch
    , testProperty "cond picks columns" prop_refCond
    , testProperty "bimapM f g m == fromF g . m . tr (fromF f)" prop_refBimap
    , testProperty "addition across Join/Fork layouts" prop_refMixedAdd
    , testProperty "Par.compWith with negative depth == comp" prop_refParNegative
    ]

toDM :: Matrix e a b -> DM.Matrix e
toDM = DM.fromLists . toLists

dmApproxEq :: Double -> DM.Matrix Double -> DM.Matrix Double -> Bool
dmApproxEq tol a b =
  DM.nrows a == DM.nrows b
    && DM.ncols a == DM.ncols b
    && all (\(x, y) -> abs (x - y) < tol) (zip (DM.toList a) (DM.toList b))

prop_refMult :: Property
prop_refMult =
  forAll ((,) <$> genSquareMatrix @S10 <*> genSquareMatrix @S10) $ \(a, b) ->
    let laopResult = toDM (comp a b)
        dmResult = DM.multStd (toDM a) (toDM b)
     in dmApproxEq 1e-6 laopResult dmResult

prop_refTranspose :: Property
prop_refTranspose =
  forAll (genMatrix @S5 @S10) $ \m ->
    toDM (tr m) == DM.transpose (toDM m)

prop_refAddition :: Property
prop_refAddition =
  forAll ((,) <$> genSquareMatrix @S10 <*> genSquareMatrix @S10) $ \(a, b) ->
    toDM (a .+. b) == DM.elementwise (+) (toDM a) (toDM b)

prop_refScalar :: Property
prop_refScalar =
  forAll ((,) <$> (arbitrary :: Gen Double) <*> genSquareMatrix @S10) $ \(s, m) ->
    dmApproxEq 1e-10 (toDM (s .| m)) (DM.scaleMatrix s (toDM m))

prop_refIdentity :: Property
prop_refIdentity =
  property $
    toDM (iden :: Matrix Double S10 S10) == DM.identity 10

prop_refMultInt :: Property
prop_refMultInt =
  forAll ((,) <$> genIntMatrix @S5 @S5 <*> genIntMatrix @S5 @S5) $ \(a, b) ->
    toDM (comp a b) == DM.multStd (toDM a) (toDM b)

-- Structural combinators against list-level definitions (Int, so exact).

-- Rows of a (p x m) and b (q x n) give a (p*q) x (m*n) block matrix.
kroneckerLists :: [[Int]] -> [[Int]] -> [[Int]]
kroneckerLists a b = [[x * y | x <- ra, y <- rb] | ra <- a, rb <- b]

prop_refKronecker :: Property
prop_refKronecker =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S2 @S3) $ \(a, b) ->
    toLists (a >< b) == kroneckerLists (toLists a) (toLists b)

prop_refKhatriRao :: Property
prop_refKhatriRao =
  forAll ((,) <$> genIntMatrix @S4 @S3 <*> genIntMatrix @S4 @S2) $ \(a, b) ->
    toLists (kr a b) == [zipWith (*) ra rb | ra <- toLists a, rb <- toLists b]

prop_refDirectSum :: Property
prop_refDirectSum =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S2 @S4) $ \(a, b) ->
    toLists (a -|- b)
      == [ra ++ replicate 2 0 | ra <- toLists a] ++ [replicate 3 0 ++ rb | rb <- toLists b]

prop_refSelect :: Property
prop_refSelect =
  forAll ((,) <$> genIntMatrix @S3 @(Either S2 S4) <*> genIntMatrix @S2 @S4) $ \(m, y) ->
    select m y == comp (join y iden) m

prop_refBranch :: Property
prop_refBranch =
  forAll ((,,) <$> genIntMatrix @S3 @(Either S2 S4) <*> genIntMatrix @S2 @S3 <*> genIntMatrix @S4 @S3) $
    \(x, l, r) -> branch x l r == comp l (comp p1 x) .+. comp r (comp p2 x)

prop_refCond :: Property
prop_refCond =
  forAll ((,) <$> genIntMatrix @S4 @S3 <*> genIntMatrix @S4 @S3) $ \(f, g) ->
    let p = even . fromEnum
        pick = [p (fromOrd @S4 c) | c <- [0 .. 3]]
        expected = transpose [if t then cf else cg | (t, cf, cg) <- zip3 pick (transpose (toLists f)) (transpose (toLists g))]
     in toLists (cond p f g) == expected

prop_refBimap :: Property
prop_refBimap =
  forAll (genIntMatrix @S3 @S4) $ \m ->
    let f = toEnum . (`mod` 2) . fromEnum :: S3 -> S2
        g = toEnum . (\i -> (i + 2) `mod` 5) . fromEnum :: S4 -> S5
     in bimapM f g m == comp (fromF g) (comp m (tr (fromF f)))

prop_refMixedAdd :: Property
prop_refMixedAdd =
  forAll ((,) <$> genIntMatrix @(Either S2 S3) @(Either S3 S2) <*> genIntMatrix @(Either S2 S3) @(Either S3 S2)) $
    \(a, b) ->
      toLists (a .+. abideFJ b) == zipWith (zipWith (+)) (toLists a) (toLists b)
        && a == abideFJ a

prop_refParNegative :: Property
prop_refParNegative =
  forAll ((,) <$> genIntMatrix @S4 @S4 <*> genIntMatrix @S4 @S4) $ \(a, b) ->
    Par.compWith (-1) a b == comp a b
