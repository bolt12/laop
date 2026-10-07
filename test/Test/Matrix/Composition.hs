{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- Composition checked bit for bit against the algorithm the library used
-- before, on every layout an operand can have. Rewrite rules stay enabled
-- here: none of them matches a product of generated operands, and the
-- specialisations of comp are themselves rewrite rules.
module Test.Matrix.Composition (compositionTests) where

import           GHC.Float             (castDoubleToWord64)
import           LAoP.Matrix.Indexed   (MatIndex (..))
import           LAoP.Matrix.Internal  (Boolean (..), Dim (..), Matrix (..),
                                        SDim)
import qualified LAoP.Matrix.Internal  as I
import           Test.Generators       (type S10, type S2, type S3, type S5)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

compositionTests :: TestTree
compositionTests =
  testGroup "Composition"
    [ shape @S5 @S5 @S5 "5x5 times 5x5"
    , shape @S3 @S10 @S2 "10x3 times 2x10"
    , shape @(Either () S3) @(Either (Either S2 ()) ()) @(Either () (Either () ())) "unbalanced trees"
    , shape @(S2, S3) @Bool @(S3, S2) "pairs and Bool"
    , shape @() @S5 @S3 "column times row"
    , shape @S5 @S3 @() "row times column"
    , shape @() @() @() "1x1"
    ]

-- Checks a product of an (a -> b) matrix and a (b -> c) matrix, that is a
-- |c| by |b| matrix times a |b| by |a| one.
shape :: forall a b c. (MatIndex a, MatIndex b, MatIndex c) => String -> TestTree
shape name =
  testGroup name
    [ testProperty "Double: comp computes the old sums, bit for bit" $
        forAll (operands genDouble) $ \(x, y) -> sameBits (I.comp x y) (compRef x y)
    , testProperty "Int: comp computes the old sums" $
        forAll (operands (arbitrary :: Gen Int)) $ \(x, y) -> I.toList (I.comp x y) == I.toList (compRef x y)
    , testProperty "Boolean: comp computes the old sums" $
        forAll (operands genBoolean) $ \(x, y) -> I.toList (I.comp x y) == I.toList (compRef x y)
    , testProperty "parCompWith d, every d, bit for bit" $
        forAll ((,) <$> operands genDouble <*> depths) $ \((x, y), d) ->
          sameBits (I.parCompWith d x y) (I.comp x y)
    , testProperty "parComp, bit for bit" $
        forAll (operands genDouble) $ \(x, y) -> sameBits (I.parComp x y) (I.comp x y)
    , testProperty "Boolean: parCompWith d" $
        forAll ((,) <$> operands genBoolean <*> depths) $ \((x, y), d) ->
          I.toList (I.parCompWith d x y) == I.toList (I.comp x y)
    , testProperty "rowsWithColumns dot on any layout is comp, bit for bit" $
        forAll (operands genDouble) $ \(x, y) -> sameBits (I.rowsWithColumns I.dot x y) (I.comp x y)
    ]
  where
    operands :: Gen e -> Gen (Matrix e (DimOf b) (DimOf c), Matrix e (DimOf a) (DimOf b))
    operands g = (,) <$> genLaidOut (dimOf @b) (dimOf @c) g <*> genLaidOut (dimOf @a) (dimOf @b) g

-- 64 is deeper than any tree here, so every split runs through the combinator.
depths :: Gen Int
depths = elements [-1, 0, 1, 2, 3, 5, 8, 64]

-- The composition of 0.2 and of the first 0.3 drafts: contract the shared
-- dimension first, add the partial products element by element.
compRef :: (Num e) => Matrix e cr rows -> Matrix e cols cr -> Matrix e cols rows
compRef (One a) (One b)       = One (a * b)
compRef (Join a b) (Fork c d) = compRef a c I..+. compRef b d
compRef (Fork a b) c          = Fork (compRef a c) (compRef b c)
compRef c (Join a b)          = Join (compRef c a) (compRef c b)

-- Equal bits, counting every NaN as the same value: the hardware may return
-- either operand's payload when both are NaN.
sameBits :: Matrix Double c r -> Matrix Double c r -> Bool
sameBits x y = map key (I.toList x) == map key (I.toList y)
  where
    key d
      | isNaN d = Nothing
      | otherwise = Just (castDoubleToWord64 d)

genDouble :: Gen Double
genDouble =
  frequency
    [ (12, arbitrary)
    , (1, elements [0, -0, 1, -1, 1e308, -1e308, 1 / 0, -(1 / 0), 0 / 0])
    ]

genBoolean :: Gen Boolean
genBoolean = do
  p <- elements [1, 5, 9 :: Int]
  (\k -> Boolean (k < p)) <$> choose (0, 9)

-- A matrix of the given shape with its Join and Fork nodes in a random order.
genLaidOut :: SDim c -> SDim r -> Gen e -> Gen (Matrix e c r)
genLaidOut sc sr g = do
  rs <- vectorOf (I.sizeOf sr) (vectorOf (I.sizeOf sc) g)
  scramble (I.withKnownDim sc (I.withKnownDim sr (I.fromLists rs)))

-- Views of a matrix's row and column blocks, whatever its layout.
data Rows e c r where
  NoRows :: Rows e c U
  RowsV :: Matrix e c r1 -> Matrix e c r2 -> Rows e c (r1 :+: r2)

data Cols e c r where
  NoCols :: Cols e U r
  ColsV :: Matrix e c1 r -> Matrix e c2 r -> Cols e (c1 :+: c2) r

rowsV :: Matrix e c r -> Rows e c r
rowsV (One _) = NoRows
rowsV (Fork t b) = RowsV t b
rowsV (Join l r) = case (rowsV l, rowsV r) of
  (NoRows, NoRows)           -> NoRows
  (RowsV lt lb, RowsV rt rb) -> RowsV (Join lt rt) (Join lb rb)

colsV :: Matrix e c r -> Cols e c r
colsV (One _) = NoCols
colsV (Join l r) = ColsV l r
colsV (Fork t b) = case (colsV t, colsV b) of
  (NoCols, NoCols)           -> NoCols
  (ColsV tl tr, ColsV bl br) -> ColsV (Fork tl bl) (Fork tr br)

scramble :: Matrix e c r -> Gen (Matrix e c r)
scramble m = case (rowsV m, colsV m) of
  (NoRows, NoCols) -> pure m
  (RowsV t b, NoCols) -> Fork <$> scramble t <*> scramble b
  (NoRows, ColsV l r) -> Join <$> scramble l <*> scramble r
  (RowsV t b, ColsV l r) ->
    oneof [Fork <$> scramble t <*> scramble b, Join <$> scramble l <*> scramble r]
