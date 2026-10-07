{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- The laws below are the left-hand sides of the library's RULES. With rules
-- enabled GHC rewrites each one to x == x, so the tests would check nothing.
-- GHC's specialiser applies imported RULES even with rules disabled, and
-- -ddump-rule-firings does not show it, so it is off too.
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-specialise #-}

module Test.Relation.Properties (relationPropertyTests) where

import           Data.List             (transpose)
import           LAoP.Category
import           LAoP.Index
import           LAoP.Matrix.Indexed   (MatIndex)
import qualified LAoP.Matrix.Indexed   as IX
import qualified LAoP.Matrix.Internal  as I
import qualified LAoP.Relation         as R
import           Prelude               hiding (id, (.))
import           Test.Generators
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (forAll)

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
    , testGroup "Documented laws"
        [ testProperty "shunting: (f . r) `sse` s == r `sse` (conv f . s)" prop_shuntLeft
        , testProperty "shunting: (r . conv f) `sse` s == r `sse` (s . f)" prop_shuntRight
        , testProperty "(r `union` s) `sse` x == (r `sse` x && s `sse` x)" prop_unionSse
        , testProperty "equalizer r r == domain r" prop_equalizerDomain
        , testProperty "equalizer f f == id for a function f" prop_equalizerFunction
        , testProperty "guard p == i2 `overriddenBy` (i1 . predR p)" prop_guard
        , testProperty "cond p r s picks r where p holds, s elsewhere" prop_cond
        ]
    , testGroup "Division and overriding"
        [ testProperty "(z . y) `sse` x == z `sse` divR x y" prop_divRGalois
        , testProperty "divR computes the four clauses of 0.2, on any layout" prop_divRReference
        , testProperty "(x . z) `sse` y == z `sse` divL x y" prop_divLGalois
        , testProperty "divL x y == conv (divR (conv y) (conv x))" prop_divLDual
        , testProperty "zeros `overriddenBy` s == s" prop_overrideZerosLeft
        , testProperty "r `overriddenBy` zeros == r" prop_overrideZerosRight
        , testProperty "r `overriddenBy` r == r" prop_overrideSelf
        ]
    , testGroup "Lookups and reindexing"
        [ testProperty "fromRel r a b is the entry of r at column a, row b" prop_fromRel
        , testProperty "pt r a lists the rows set in column a" prop_pt
        , testProperty "trans r == splitR r sndR . conv fstR" prop_trans
        , testProperty "untrans s == fstR . conv (splitR (conv s) sndR)" prop_untrans
        , testProperty "untrans (trans r) == r" prop_untransTrans
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

-- IX.toRel f has f a b at row b, column a, so composing two of them is
-- relational composition: some middle value links the input to the output.
prop_indexedCompRel :: Property
prop_indexedCompRel =
  forAllBlind ((,) <$> arbitrary <*> arbitrary) $ \(f, g :: S4 -> S4 -> Bool) ->
    IX.toLists (IX.comp (IX.toRel f) (IX.toRel g))
      == [[R.Boolean (or [g a b && f b c | b <- universe]) | a <- universe] | c <- universe]
  where
    universe = [minBound .. maxBound]

prop_booleanShow :: Property
prop_booleanShow = property $ show (1 :: R.Boolean) == "1" && show (0 :: R.Boolean) == "0"

-- Pairing

prop_splitRProjections :: Property
prop_splitRProjections =
  forAll ((,) <$> genRelation @S3 @S2 <*> genRelation @S3 @S4) $ \(r, s) ->
    R.comp R.fstR (R.splitR r s) `R.sse` r && R.comp R.sndR (R.splitR r s) `R.sse` s

-- Documented laws

genFunction :: forall a b. (MatIndex a, CoArbitrary a, Arbitrary b, MatIndex b) => Gen (R.Relation a b)
genFunction = R.fromF <$> (arbitrary :: Gen (a -> b))

prop_shuntLeft :: Property
prop_shuntLeft =
  forAll ((,,) <$> genFunction @S3 @S4 <*> genRelation @S2 @S3 <*> genRelation @S2 @S4) $ \(f, r, s) ->
    ((f . r) `R.sse` s) == (r `R.sse` (R.conv f . s))

prop_shuntRight :: Property
prop_shuntRight =
  forAll ((,,) <$> genFunction @S3 @S4 <*> genRelation @S3 @S2 <*> genRelation @S4 @S2) $ \(f, r, s) ->
    ((r . R.conv f) `R.sse` s) == (r `R.sse` (s . f))

prop_unionSse :: Property
prop_unionSse =
  forAll ((,,) <$> genRelation @S3 @S4 <*> genRelation @S3 @S4 <*> genRelation @S3 @S4) $ \(r, s, x) ->
    ((r `R.union` s) `R.sse` x) == (r `R.sse` x && s `R.sse` x)

prop_equalizerDomain :: Property
prop_equalizerDomain =
  forAll (genRelation @S3 @S4) $ \r -> R.equalizer r r == R.domain r

prop_equalizerFunction :: Property
prop_equalizerFunction =
  forAll (genFunction @S3 @S4) $ \f -> R.equalizer f f == id

prop_guard :: Property
prop_guard =
  forAll (genRelation @S4 @Bool) $ \p ->
    R.guard p == (R.i2 `R.overriddenBy` (R.i1 . R.predR p))

prop_cond :: Property
prop_cond =
  forAll ((,,) <$> genRelation @S3 @Bool <*> genRelation @S3 @S4 <*> genRelation @S3 @S4) $ \(p, r, s) ->
    let at q x y = R.fromRel q x y
        expected x y = (at p x True && at r x y) || (not (at p x True) && at s x y)
     in R.cond p r s == R.toRel expected

-- Division and overriding

prop_divRGalois :: Property
prop_divRGalois =
  forAll ((,,) <$> genRelation @S3 @S4 <*> genRelation @S3 @S2 <*> genRelation @S2 @S4) $ \(x, y, z) ->
    ((z . y) `R.sse` x) == (z `R.sse` R.divR x y)

-- The relational right division of 0.2, the four clauses of comp with
-- conjunction for addition and implication for multiplication.
divRReference :: I.Matrix I.Boolean b c -> I.Matrix I.Boolean b a -> I.Matrix I.Boolean a c
divRReference (I.One a)    (I.One b)    = I.One (I.fromBool (not (I.toBool b) || I.toBool a))
divRReference (I.Join a b) (I.Join c d) = divRReference a c I..*. divRReference b d
divRReference (I.Fork a b) c            = I.Fork (divRReference a c) (divRReference b c)
divRReference c            (I.Fork a b) = I.Join (divRReference c a) (divRReference c b)

-- Both operands are checked as generated, row by row, and laid out column by
-- column: the converse of their converse built row by row from the transposed
-- rows, which involves no product.
prop_divRReference :: Property
prop_divRReference =
  forAll ((,) <$> genRelation @S3 @S4 <*> genRelation @S3 @S2) $ \(x@(R.R (IX.M x')), y@(R.R (IX.M y'))) ->
    let expected = R.R (IX.M (divRReference x' y'))
     in R.divR x y == expected && R.divR (byColumns x) (byColumns y) == expected
  where
    byColumns :: (MatIndex a, MatIndex b) => R.Relation a b -> R.Relation a b
    byColumns r = R.conv (R.fromLists (transpose (R.toLists r)))

prop_divLGalois :: Property
prop_divLGalois =
  forAll ((,,) <$> genRelation @S4 @S3 <*> genRelation @S2 @S3 <*> genRelation @S2 @S4) $ \(x, y, z) ->
    ((x . z) `R.sse` y) == (z `R.sse` R.divL x y)

prop_divLDual :: Property
prop_divLDual =
  forAll ((,) <$> genRelation @S4 @S3 <*> genRelation @S2 @S3) $ \(x, y) ->
    R.divL x y == R.conv (R.divR (R.conv y) (R.conv x))

prop_overrideZerosLeft :: Property
prop_overrideZerosLeft =
  forAll (genRelation @S3 @S4) $ \s -> (R.zeros `R.overriddenBy` s) == s

prop_overrideZerosRight :: Property
prop_overrideZerosRight =
  forAll (genRelation @S3 @S4) $ \r -> (r `R.overriddenBy` R.zeros) == r

prop_overrideSelf :: Property
prop_overrideSelf =
  forAll (genRelation @S3 @S4) $ \r -> (r `R.overriddenBy` r) == r

-- Lookups and reindexing

-- The element of a relation at column a and row b, read off its rows.
entry :: (MatIndex a, MatIndex b) => R.Relation a b -> a -> b -> Bool
entry r a b = R.toLists r !! IX.toOrd b !! IX.toOrd a == R.Boolean True

prop_fromRel :: Property
prop_fromRel =
  forAll ((,,) <$> genRelation @S3 @S4 <*> arbitrary <*> arbitrary) $ \(r, a :: S3, b :: S4) ->
    R.fromRel r a b == entry r a b

prop_pt :: Property
prop_pt =
  forAll ((,) <$> genRelation @S3 @S4 <*> arbitrary) $ \(r, a :: S3) ->
    let L bs = R.pt r a
     in bs == [b | b <- [minBound .. maxBound], entry r a b]

prop_trans :: Property
prop_trans =
  forAll (genRelation @(S2, S3) @S4) $ \r ->
    R.trans r == (R.splitR r R.sndR . R.conv R.fstR)

prop_untrans :: Property
prop_untrans =
  forAll (genRelation @S2 @(S4, S3)) $ \s ->
    R.untrans s == (R.fstR . R.conv (R.splitR (R.conv s) R.sndR))

prop_untransTrans :: Property
prop_untransTrans =
  forAll (genRelation @(S2, S3) @S4) $ \r -> R.untrans (R.trans r) == r
