{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- bindD d iden is comp iden d once bindD inlines, which the library's RULES
-- would rewrite to d, so the monad-law tests would check nothing.
-- GHC's specialiser applies imported RULES even with rules disabled, and
-- -ddump-rule-firings does not show it, so it is off too.
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-specialise #-}

module Test.Dist.Properties (distPropertyTests) where

import           Data.List             (isInfixOf)
import           Data.List.NonEmpty    (NonEmpty (..))
import           LAoP.Category
import           LAoP.Dist.Internal
import           LAoP.Matrix.Indexed   (MatIndex, comp, fromF, iden)
import           Prelude               hiding (id, (.))
import           Test.Generators
import           Test.QuickCheck       hiding (choose)
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (choose, forAll)

distPropertyTests :: TestTree
distPropertyTests =
  testGroup "Distribution properties"
    [ testProperty "probabilities sum to 1" prop_sumToOne
    , testProperty "fmapD id d == d" prop_fmapDId
    , testProperty "choose 0.5 gives equal probs" prop_choose
    , testProperty "returnD x is point distribution" prop_returnD
    , testProperty "(??) (const True) d == 1" prop_tautology
    , testProperty "(??) (const False) d == 0" prop_contradiction
    , testProperty "uniform gives equal probs" prop_uniform
    , testProperty "uniform adds up repeated outcomes" prop_uniformDuplicates
    , testProperty "linear on one outcome is a point distribution" prop_linearSingleton
    , testProperty "choose p gives p to the outcome numbered 0" prop_chooseOrientation
    , testProperty "fromFreqs normalises and adds up duplicates" prop_fromFreqs
    , testProperty "fmapD composes" prop_fmapDCompose
    , testProperty "fmapD f is composition with fromF f" prop_fmapDFromF
    , testProperty "show gives the fromFreqs call" prop_showDist
    , testProperty "choose rejects a probability outside [0, 1]" prop_chooseRange
    , testProperty "fromFreqs rejects a negative weight" prop_fromFreqsNegative
    , testGroup "Column-stochastic matrices keep distributions (Oliveira 2012)"
        [ testProperty "bindD" prop_bindDStochastic
        , testProperty "multD" prop_multDStochastic
        , testProperty "selectD" prop_selectDStochastic
        ]
    , testGroup "Monad laws as matrix laws"
        [ testProperty "bindD d iden == d" prop_bindDRightIdentity
        , testProperty "bindD (bindD d g) f == bindD d (f . g)" prop_bindDAssoc
        ]
    ]

prop_sumToOne :: Property
prop_sumToOne =
  forAll (genDist @S5) $ \d ->
    let s = sum (map snd (toValues d))
     in abs (s - 1) < 1e-10

prop_fmapDId :: Property
prop_fmapDId =
  forAll (genDist @S5) $ \d ->
    fmapD id d == d

prop_choose :: Property
prop_choose =
  property $
    let d = choose @Bool 0.5
        vs = map snd (toValues d)
     in all (\p -> abs (p - 0.5) < 1e-10) vs

prop_returnD :: Property
prop_returnD =
  property $
    let d = returnD True
        vs = toValues d
        trueProb = sum [p | (v, p) <- vs, v]
        falseProb = sum [p | (v, p) <- vs, not v]
     in abs (trueProb - 1) < 1e-10 && abs falseProb < 1e-10

prop_tautology :: Property
prop_tautology =
  forAll (genDist @S5) $ \d ->
    abs (const True ?? d - 1) < 1e-10

prop_contradiction :: Property
prop_contradiction =
  forAll (genDist @S5) $ \d ->
    abs (const False ?? d) < 1e-10

prop_uniform :: Property
prop_uniform =
  property $
    let d = uniform (False :| [True])
        vs = map snd (toValues d)
     in all (\p -> abs (p - 0.5) < 1e-10) vs

prop_uniformDuplicates :: Property
prop_uniformDuplicates =
  property $
    let d = uniform (True :| [True, False])
     in abs ((id ?? d) - 2 / 3) < 1e-10 && abs (sum (map snd (toValues d)) - 1) < 1e-10

prop_linearSingleton :: Property
prop_linearSingleton = property $ linear (True :| []) == returnD True

prop_chooseOrientation :: Property
prop_chooseOrientation =
  property $
    let d = choose @Bool 0.9
     in abs ((not ?? d) - 0.9) < 1e-10 && abs ((id ?? d) - 0.1) < 1e-10

prop_fromFreqs :: Property
prop_fromFreqs =
  property $
    toValues (fromFreqs [(False, 1), (True, 2), (False, 1)])
      == [(False, 0.5), (True, 0.5)]

prop_fmapDCompose :: Property
prop_fmapDCompose =
  forAll (genDist @S5) $ \d ->
    let f = toEnum . (`mod` 3) . fromEnum :: S5 -> S3
        g = even . fromEnum :: S3 -> Bool
     in all (\(x, y) -> fst x == fst y && abs (snd x - snd y) < 1e-10)
          (zip (toValues (fmapD g (fmapD f d))) (toValues (fmapD (g . f) d)))

prop_chooseRange :: Property
prop_chooseRange =
  ioProperty $ do
    msg <- errorOf (choose @Bool 1.5)
    pure (maybe False ("1.5 is outside [0, 1]" `isInfixOf`) msg)

prop_fromFreqsNegative :: Property
prop_fromFreqsNegative =
  ioProperty $ do
    msg <- errorOf (fromFreqs [(False, -1), (True, 2)])
    pure (maybe False ("-1.0 is not >= 0" `isInfixOf`) msg)

-- Distributions

isDist :: (MatIndex a) => Dist a -> Bool
isDist d =
  let ps = map snd (toValues d)
   in all (>= 0) ps && abs (sum ps - 1) < 1e-9

prop_bindDStochastic :: Property
prop_bindDStochastic =
  forAll ((,) <$> genDist @S5 <*> genStochastic @S5 @S3) $ \(d, m) ->
    isDist (bindD d m)

prop_multDStochastic :: Property
prop_multDStochastic =
  forAll ((,) <$> genDist @S3 <*> genDist @S4) $ \(d, e) ->
    isDist (multD d e)

prop_selectDStochastic :: Property
prop_selectDStochastic =
  forAll ((,) <$> genDist @(Either S2 S3) <*> genStochastic @S2 @S3) $ \(d, m) ->
    isDist (selectD d m)

prop_bindDRightIdentity :: Property
prop_bindDRightIdentity =
  forAll (genDist @S5) $ \d ->
    bindD d iden == d

prop_bindDAssoc :: Property
prop_bindDAssoc =
  forAll ((,,) <$> genDist @S3 <*> genStochastic @S3 @S4 <*> genStochastic @S4 @S2) $ \(d, g, f) ->
    let D x = bindD (bindD d g) f
        D y = bindD d (f . g)
     in approxEqual 1e-9 x y

prop_fmapDFromF :: Property
prop_fmapDFromF =
  forAll (genDist @S5) $ \d ->
    let f = toEnum . (`mod` 3) . fromEnum :: S5 -> S3
        viaMatrix = toValues (D (fromF f `comp` toMatrix d))
     in all (\(x, y) -> fst x == fst y && abs (snd x - snd y) < 1e-12) (zip (toValues (fmapD f d)) viaMatrix)

prop_showDist :: Property
prop_showDist =
  forAll (genDist @S3) $ \d -> show d == "fromFreqs " ++ show (toValues d)
