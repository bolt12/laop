{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- The laws below are the left-hand sides of the library's RULES. With rules
-- enabled GHC rewrites each one to x == x, so the tests would check nothing.
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Test.Relation.Properties (relationPropertyTests) where

import qualified LAoP.Matrix.Indexed    as IX
import qualified LAoP.Relation.Internal as R
import           LAoP.Utils
import           Prelude                hiding (id, (.))
import           Test.Generators
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck  hiding (forAll)

relationPropertyTests :: TestTree
relationPropertyTests =
  testGroup "Relation properties"
    [ testGroup "Converse"
        [ testProperty "conv involution" prop_convInvolution
        , testProperty "conv contravariant" prop_convContravariant
        ]
    , testGroup "fromF"
        [ testProperty "fromF id == iden" prop_fromFId
        , testProperty "fromF is a function (simple & entire)" prop_fromFIsFunction
        ]
    , testGroup "Boolean algebra"
        [ testProperty "r `intersection` complement r == zeros" prop_intersectComplement
        , testProperty "r `union` complement r == ones" prop_unionComplement
        , testProperty "r `sse` union r s" prop_sseUnion
        ]
    , testGroup "Endo-relational"
        [ testProperty "iden is reflexive" prop_idenReflexive
        , testProperty "ker r is symmetric" prop_kerSymmetric
        , testProperty "ker (fromF f) is equivalence" prop_kerFromFEquivalence
        , testProperty "zeros and ones are partial equivalences" prop_perConstants
        , testProperty "predR p is a partial equivalence" prop_perPredR
        , testProperty "a PER need not be reflexive" prop_perNotReflexive
        ]
    , testGroup "Pairing"
        [ testProperty "fstR . splitR r s `sse` r, sndR . splitR r s `sse` s" prop_splitRProjections
        ]
    , testGroup "Composition"
        [ testProperty "comp matches the list definition" prop_compReference
        , testProperty "Indexed comp on toRel output is relational" prop_indexedCompRel
        , testProperty "Boolean shows as 0/1" prop_booleanShow
        ]
    ]

-- Converse

prop_convInvolution :: Property
prop_convInvolution =
  forAll (genRelation @S4 @S4) $ \r ->
    R.conv (R.conv r) == r

prop_convContravariant :: Property
prop_convContravariant =
  forAll ((,) <$> genRelation @S4 @S4 <*> genRelation @S4 @S4) $ \(r, s) ->
    R.conv (R.comp r s) == R.comp (R.conv s) (R.conv r)

-- fromF

prop_fromFId :: Property
prop_fromFId =
  property $
    (R.fromF id :: R.Relation S4 S4) == (id :: R.Relation S4 S4)

prop_fromFIsFunction :: Property
prop_fromFIsFunction =
  property $ R.function (R.fromF not :: R.Relation Bool Bool)

-- Boolean algebra

prop_intersectComplement :: Property
prop_intersectComplement =
  forAll (genRelation @S4 @S4) $ \r ->
    R.intersection r (R.complement r) == R.zeros

prop_unionComplement :: Property
prop_unionComplement =
  forAll (genRelation @S4 @S4) $ \r ->
    R.union r (R.complement r) == R.ones

prop_sseUnion :: Property
prop_sseUnion =
  forAll ((,) <$> genRelation @S4 @S4 <*> genRelation @S4 @S4) $ \(r, s) ->
    R.sse r (R.union r s)

-- Endo-relational

prop_idenReflexive :: Property
prop_idenReflexive =
  property $ R.reflexive (id :: R.Relation S4 S4)

prop_kerSymmetric :: Property
prop_kerSymmetric =
  forAll (genRelation @S4 @S4) $ \r ->
    R.symmetric (R.ker r)

prop_kerFromFEquivalence :: Property
prop_kerFromFEquivalence =
  property $ R.equivalence (R.ker (R.fromF not :: R.Relation Bool Bool))

-- Partial equivalences

prop_perConstants :: Property
prop_perConstants =
  property $
    R.partialEquivalence (R.zeros :: R.Relation S4 S4)
      && R.partialEquivalence (R.ones :: R.Relation S4 S4)

prop_perPredR :: Property
prop_perPredR =
  property $ R.partialEquivalence (R.predR (R.fromF (even . fromEnum) :: R.Relation S4 Bool))

prop_perNotReflexive :: Property
prop_perNotReflexive =
  property $ not (R.reflexive (R.zeros :: R.Relation S4 S4))

-- Composition

-- (r . s) relates a to c iff some b has s a b and r b c.
composeLists :: [[R.Boolean]] -> [[R.Boolean]] -> [[R.Boolean]]
composeLists r s =
  [ [ sum [x * y | (x, y) <- zip rowR colS] | colS <- transpose' s ] | rowR <- r ]
  where
    transpose' []       = []
    transpose' ([] : _) = []
    transpose' xs       = map head' xs : transpose' (map (drop 1) xs)
    head' (x : _) = x
    head' []      = 0

prop_compReference :: Property
prop_compReference =
  forAll ((,) <$> genRelation @S4 @S3 <*> genRelation @S5 @S4) $ \(r, s) ->
    R.toLists (R.comp r s) == composeLists (R.toLists r) (R.toLists s)

prop_indexedCompRel :: Property
prop_indexedCompRel =
  forAll ((,) <$> genRelation @S4 @S4 <*> genRelation @S4 @S4) $ \(R.R r, R.R s) ->
    IX.toLists (IX.comp r s) == R.toLists (R.comp (R.R r) (R.R s))

prop_booleanShow :: Property
prop_booleanShow = property $ show (1 :: R.Boolean) == "1" && show (0 :: R.Boolean) == "0"

-- Pairing

prop_splitRProjections :: Property
prop_splitRProjections =
  forAll ((,) <$> genRelation @S3 @S2 <*> genRelation @S3 @S4) $ \(r, s) ->
    R.comp R.fstR (R.splitR r s) `R.sse` r && R.comp R.sndR (R.splitR r s) `R.sse` s
