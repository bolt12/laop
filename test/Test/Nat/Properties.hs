{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoStarIsType        #-}

module Test.Nat.Properties (natPropertyTests) where

import           Data.List                (transpose)
import           Data.Proxy               (Proxy (..))
import           GHC.TypeLits             (KnownNat, natVal)
import           LAoP.Matrix.Nat
import qualified LAoP.Matrix.Parallel.Nat as Par
import           LAoP.Utils               (Category (..))
import           Prelude                  hiding (id, (.))
import           Test.QuickCheck          hiding ((===), (><))
import           Test.Tasty
import           Test.Tasty.QuickCheck    hiding ((===), (><))

natPropertyTests :: TestTree
natPropertyTests =
  testGroup "Nat matrices"
    [ testGroup "Blocks of any size"
        [ testProperty "join 1 4" prop_join14
        , testProperty "join 2 1" prop_join21
        , testProperty "fork 3 2" prop_fork32
        , testProperty "p1 . fork a b == a for 3 + 2" prop_p1Fork
        , testProperty "p2 . fork a b == b for 3 + 2" prop_p2Fork
        , testProperty "direct sum 3 1" prop_directSum
        , testProperty "select with a 3 + 2 split" prop_select
        , testProperty "splitJoin undoes join for 1 + 4" prop_splitJoin14
        , testProperty "splitFork undoes fork for 3 + 2" prop_splitFork32
        ]
    , testGroup "Products of any size"
        [ testProperty "kr 3 2" prop_kr32
        , testProperty "Kronecker 3x2 by 2x3" prop_kronecker
        , testProperty "fstM . kr a b == a when b is stochastic" prop_fstMKr
        ]
    , testGroup "Builders"
        [ testProperty "fromF maps column c to row f c" prop_fromF
        , testProperty "matrixBuilder' receives (row, column)" prop_builderOrder
        , testProperty "cond picks columns" prop_cond
        , testProperty "point" prop_point
        ]
    , testGroup "Composition"
        [ testProperty "Category identity" prop_category
        , testProperty "Par.comp == comp" prop_parComp
        ]
    ]

-- Generators

natInt :: forall n. (KnownNat n) => Int
natInt = fromInteger (natVal (Proxy @n))

genNat :: forall c r. (KnownNat c, KnownNat r) => Gen (Matrix Int c r)
genNat = fromLists <$> vectorOf (natInt @r) (vectorOf (natInt @c) (choose (-9, 9)))

-- Blocks

prop_join14 :: Property
prop_join14 =
  forAll ((,) <$> genNat @1 @2 <*> genNat @4 @2) $ \(a, b) ->
    toLists (join a b) == zipWith (++) (toLists a) (toLists b)

prop_join21 :: Property
prop_join21 =
  forAll ((,) <$> genNat @2 @3 <*> genNat @1 @3) $ \(a, b) ->
    toLists (a ||| b) == zipWith (++) (toLists a) (toLists b)

prop_fork32 :: Property
prop_fork32 =
  forAll ((,) <$> genNat @2 @3 <*> genNat @2 @2) $ \(a, b) ->
    toLists (a === b) == toLists a ++ toLists b

prop_p1Fork :: Property
prop_p1Fork =
  forAll ((,) <$> genNat @4 @3 <*> genNat @4 @2) $ \(a, b) ->
    comp (p1 @Int @3 @2) (fork a b) == a

prop_p2Fork :: Property
prop_p2Fork =
  forAll ((,) <$> genNat @4 @3 <*> genNat @4 @2) $ \(a, b) ->
    comp (p2 @Int @3 @2) (fork a b) == b

prop_directSum :: Property
prop_directSum =
  forAll ((,) <$> genNat @3 @2 <*> genNat @1 @4) $ \(a, b) ->
    toLists (a -|- b)
      == [ra ++ [0] | ra <- toLists a] ++ [replicate 3 0 ++ rb | rb <- toLists b]

prop_select :: Property
prop_select =
  forAll ((,) <$> genNat @4 @5 <*> genNat @3 @2) $ \(m, y) ->
    select @Int @4 @3 @2 m y == comp (join y iden) m

-- 1 + 4 is not where the tree of 5 splits, so this goes through relayout.
prop_splitJoin14 :: Property
prop_splitJoin14 =
  forAll ((,) <$> genNat @1 @2 <*> genNat @4 @2) $ \(a, b) ->
    splitJoin @Int @1 @4 (join a b) == (a, b)

prop_splitFork32 :: Property
prop_splitFork32 =
  forAll ((,) <$> genNat @2 @3 <*> genNat @2 @2) $ \(a, b) ->
    splitFork @Int @2 @3 @2 (fork a b) == (a, b)

-- Products

prop_kr32 :: Property
prop_kr32 =
  forAll ((,) <$> genNat @4 @3 <*> genNat @4 @2) $ \(a, b) ->
    toLists (kr a b) == [zipWith (*) ra rb | ra <- toLists a, rb <- toLists b]

prop_kronecker :: Property
prop_kronecker =
  forAll ((,) <$> genNat @2 @3 <*> genNat @3 @2) $ \(a, b) ->
    toLists (a >< b) == [[x * y | x <- ra, y <- rb] | ra <- toLists a, rb <- toLists b]

prop_fstMKr :: Property
prop_fstMKr =
  forAll (genNat @4 @3) $ \a ->
    -- Every column of b sums to 1, so projecting away b's rows recovers a.
    let b = fromF (`mod` 2) :: Matrix Int 4 2
     in comp (fstM @Int @3 @2) (kr a b) == a

-- Builders

prop_fromF :: Property
prop_fromF =
  property $
    toLists (fromF (\c -> (c + 1) `mod` 3) :: Matrix Int 3 3)
      == [[0, 0, 1], [1, 0, 0], [0, 1, 0]]

prop_builderOrder :: Property
prop_builderOrder =
  property $
    toLists (matrixBuilder' (\(r, c) -> 10 * r + c) :: Matrix Int 3 2)
      == [[0, 1, 2], [10, 11, 12]]

prop_cond :: Property
prop_cond =
  forAll ((,) <$> genNat @5 @3 <*> genNat @5 @3) $ \(f, g) ->
    toLists (cond even f g)
      == transpose [if even c then cf else cg | (c, cf, cg) <- zip3 [0 :: Int ..] (transpose (toLists f)) (transpose (toLists g))]

prop_point :: Property
prop_point = property $ toLists (point 2 :: Matrix Int 1 4) == [[0], [0], [1], [0]]

-- Composition

prop_category :: Property
prop_category =
  forAll (genNat @3 @5) $ \m -> (id . m) == m && (m . id) == m

prop_parComp :: Property
prop_parComp =
  forAll ((,) <$> genNat @5 @6 <*> genNat @7 @5) $ \(a, b) ->
    Par.comp a b == comp a b && Par.compWith 2 a b == comp a b
