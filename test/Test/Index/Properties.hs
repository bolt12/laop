{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Index.Properties (indexPropertyTests) where

import           Data.List             (isInfixOf, nub, sort)
import           GHC.Generics          (Generic)
import           LAoP.Matrix.Indexed
import qualified LAoP.Relation         as R
import           LAoP.Utils
import           Test.Generators       (errorOf)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Read             (readMaybe)

-- An enumeration that relies on every MatIndex default, including DimOf.
data Colour = Red | Green | Blue
  deriving (Eq, Show, Generic)

instance MatIndex Colour

indexPropertyTests :: TestTree
indexPropertyTests =
  testGroup "MatIndex"
    [ testGroup "toOrd . fromOrd == id"
        [ testProperty "()" (roundTrip @())
        , testProperty "Bool" (roundTrip @Bool)
        , testProperty "Colour (generic defaults)" (roundTrip @Colour)
        , testProperty "Ranged 3 7" (roundTrip @(Ranged 3 7))
        , testProperty "Either Colour (Ranged 1 2)" (roundTrip @(Either Colour (Ranged 1 2)))
        , testProperty "(Bool, Ranged 0 2)" (roundTrip @(Bool, Ranged 0 2))
        , testProperty "BoundedList Bool" (roundTrip @(BoundedList Bool))
        , testProperty "BoundedList (Ranged 0 2)" (roundTrip @(BoundedList (Ranged 0 2)))
        ]
    , testProperty "generic default numbers constructors in order" prop_colour
    , testGroup "Ranged"
        [ testProperty "[minBound ..] stops at maxBound" prop_rangedEnumFrom
        , testProperty "[x, y ..] stops at the bound" prop_rangedEnumFromThen
        , testProperty "mkRanged reports the value and the range" prop_rangedMessage
        , testProperty "literals past Int are rejected, not wrapped" prop_rangedOverflow
        , testProperty "coerceRanged checks its result" prop_coerceChecked
        , testProperty "read rejects values outside the range" prop_rangedRead
        ]
    , testGroup "BoundedList"
        [ testProperty "Enum numbers subsets by bitmask, first value most significant" prop_powersetOrder
        , testProperty "Enum agrees with MatIndex" prop_enumAgrees
        , testProperty "[minBound ..] lists every subset once" prop_enumFrom
        , testProperty "pt belongs s == s (normalised)" prop_ptBelongs
        ]
    ]

roundTrip :: forall a. (MatIndex a) => Property
roundTrip =
  property $ all (\i -> toOrd (fromOrd @a i) == i) [0 .. cardinality @a - 1]

prop_colour :: Property
prop_colour =
  property $
    cardinality @Colour == 3
      && map toOrd [Red, Green, Blue] == [0, 1, 2]
      && map (fromOrd @Colour) [0, 1, 2] == [Red, Green, Blue]

prop_powersetOrder :: Property
prop_powersetOrder =
  property $
    map (toEnum @(BoundedList Bool)) [0 .. 3]
      == [L [], L [True], L [False], L [False, True]]

prop_enumAgrees :: Property
prop_enumAgrees =
  property $
    all
      (\i -> toOrd (toEnum @(BoundedList (Ranged 0 2)) i) == i)
      [0 .. cardinality @(BoundedList (Ranged 0 2)) - 1]

prop_enumFrom :: Property
prop_enumFrom =
  property $
    map fromEnum ([minBound ..] :: [BoundedList (Ranged 0 2)]) == [0 .. 7]

prop_ptBelongs :: Property
prop_ptBelongs =
  property $
    all
      (\s -> normalise (R.pt (R.belongs @(Ranged 0 2)) s) == normalise s)
      [fromOrd i | i <- [0 .. cardinality @(BoundedList (Ranged 0 2)) - 1]]
  where
    normalise (L xs) = sort (nub xs)

-- Ranged

value :: Ranged n m -> Int
value (Rng i) = i

prop_rangedEnumFrom :: Property
prop_rangedEnumFrom =
  property $ map value ([minBound ..] :: [Ranged 1 3]) == [1, 2, 3]

prop_rangedEnumFromThen :: Property
prop_rangedEnumFromThen =
  property $ map value ([mkRanged 1, mkRanged 3 ..] :: [Ranged 1 6]) == [1, 3, 5]

prop_rangedMessage :: Property
prop_rangedMessage =
  ioProperty $ do
    msg <- errorOf (mkRanged 5 :: Ranged 0 1)
    pure (maybe False ("5 is outside [0, 1]" `isInfixOf`) msg)

prop_rangedOverflow :: Property
prop_rangedOverflow =
  ioProperty $ do
    msg <- errorOf (fromInteger 18446744073709551617 :: Ranged 0 1)
    pure (maybe False ("outside [0, 1]" `isInfixOf`) msg)

prop_coerceChecked :: Property
prop_coerceChecked =
  ioProperty $ do
    let six = mkRanged 6 :: Ranged 1 6
    msg <- errorOf (coerceRanged (+) six six :: Ranged 2 11)
    pure (maybe False ("12 is outside [2, 11]" `isInfixOf`) msg)

prop_rangedRead :: Property
prop_rangedRead =
  property $
    (readMaybe "Rng 7" :: Maybe (Ranged 0 1)) == Nothing
      && (readMaybe (show (mkRanged 1 :: Ranged 0 1)) :: Maybe (Ranged 0 1)) == Just (mkRanged 1)
