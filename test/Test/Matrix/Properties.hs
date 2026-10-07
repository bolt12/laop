{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- The laws below are the left-hand sides of the library's RULES. With rules
-- enabled GHC rewrites each one to x == x, so the tests would check nothing.
-- GHC's specialiser applies imported RULES even with rules disabled, and
-- -ddump-rule-firings does not show it, so it is off too.
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-specialise #-}

module Test.Matrix.Properties (matrixPropertyTests) where

import           Data.Maybe            (isJust)
import           GHC.TypeLits          (KnownNat)
import           LAoP.Category
import           LAoP.Index
import           LAoP.Matrix.Indexed
import qualified LAoP.Matrix.Internal  as I
import           Prelude               hiding (id, (.))
import           Test.Generators
import           Test.QuickCheck       hiding ((===), (><))
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (forAll, (===), (><))

matrixPropertyTests :: TestTree
matrixPropertyTests =
  testGroup "Matrix properties"
    [ testGroup "Category laws"
        [ testProperty "left identity" prop_leftIdentity
        , testProperty "right identity" prop_rightIdentity
        , testProperty "associativity" prop_associativity
        ]
    , testGroup "Composition laws"
        [ testProperty "divide and conquer: join a b . fork c d == a . c + b . d" prop_divideAndConquer
        , testProperty "split fusion: fork a b . c == fork (a . c) (b . c)" prop_splitFusion
        , testProperty "junc fusion: c . join a b == join (c . a) (c . b)" prop_juncFusion
        ]
    , testGroup "Transpose"
        [ testProperty "involution" prop_trInvolution
        , testProperty "contravariant" prop_trContravariant
        , testProperty "tr iden == iden" prop_trIden
        ]
    , testGroup "Biproduct"
        [ testProperty "p1 . fork a b == a" prop_p1Fork
        , testProperty "p2 . fork a b == b" prop_p2Fork
        , testProperty "join a b . i1 == a" prop_joinI1
        , testProperty "join a b . i2 == b" prop_joinI2
        , testProperty "fork (p1 . m) (p2 . m) == m" prop_forkUniversal
        , testProperty "join (m . i1) (m . i2) == m" prop_joinUniversal
        , testProperty "splitJoin m == (m . i1, m . i2), either layout" prop_splitJoin
        , testProperty "splitFork m == (p1 . m, p2 . m), either layout" prop_splitFork
        , testProperty "biproduct: p1 . i1 == id, p2 . i2 == id" prop_biproduct
        , testProperty "orthogonality: p1 . i2 == 0, p2 . i1 == 0" prop_orthogonality
        , testProperty "i1 . p1 + i2 . p2 == id" prop_biproductSum
        , testProperty "reflexion: join i1 i2 == id, fork p1 p2 == id" prop_reflexion
        ]
    , testGroup "Bilinearity"
        [ testProperty "m . (n + l) == m . n + m . l" prop_bilinearLeft
        , testProperty "(n + l) . m == n . m + l . m" prop_bilinearRight
        ]
    , testGroup "Element-wise"
        [ testProperty "addition commutative" prop_addCommutative
        , testProperty "addition identity (zeros)" prop_addIdentity
        , testProperty "Hadamard commutative" prop_hadCommutative
        , testProperty "Hadamard identity (ones)" prop_hadIdentity
        ]
    , testGroup "Scalar"
        [ testProperty "1 .| m == m" prop_scalarIdentity
        , testProperty "distributive over addition" prop_scalarDistributive
        ]
    , testGroup "Construction"
        [ testProperty "fromLists . toLists round-trip" prop_fromToLists
        , testProperty "fromF id == iden" prop_fromFId
        , testProperty "fromF (g . f) == comp (fromF g) (fromF f)" prop_fromFComp
        ]
    , testGroup "Khatri-Rao"
        [ testProperty "kr (s .| f) g == s .| kr f g" prop_krScale
        , testProperty "kr a b == (tr fstM . a) .*. (tr sndM . b)" prop_krDefinition
        , testProperty "fstM . kr a b == a .*. (ones . b)" prop_krFstM
        , testProperty "sndM . kr a b == (ones . a) .*. b" prop_krSndM
        , testProperty "kr bang a == a == kr a bang" prop_krUnit
        , testProperty "fstM . kr a b == a, sndM . kr a b == b for column-stochastic a, b" prop_krCancel
        , testProperty "fstM . kr a b == a, sndM . kr a b == b for functions" prop_krCancelFunctions
        , testProperty "fstM . kr a b == a when b is column-stochastic" prop_krCancelFst
        , testProperty "sndM . kr a b == b when a is column-stochastic" prop_krCancelSnd
        , testProperty "fstM == iden >< bang, sndM == bang >< iden" prop_krProjections
        ]
    , testGroup "Khatri-Rao laws"
        [ testProperty "associative" prop_krAssoc
        , testProperty "kr (join m n) (join p q) == join (kr m p) (kr n q)" prop_krBlocks
        , testProperty "kr v (fork m n) == fork (kr v m) (kr v n), v a row" prop_krRowFork
        , testProperty "kr u v == u >< v for columns u, v" prop_krColumns
        , testProperty "kr v u == v .*. u for rows v, u" prop_krRows
        , testProperty "kr s iden is the diagonal of the row s" prop_krDiagonal
        , testProperty "(a >< b) . kr c d == kr (a . c) (b . d)" prop_krMixedProduct
        , testProperty "tr (kr a b) . kr c d == (tr a . c) .*. (tr b . d)" prop_krGram
        , testProperty "kr a (b .+. c) == kr a b .+. kr a c" prop_krBilinear
        , testProperty "kr m n . h == kr (m . h) (n . h) for a function h" prop_krFusion
        , testProperty "p . tr (kr q v) == kr v p . tr q" prop_krAfonso36
        , testProperty "p . tr v == kr v p . tr bang" prop_krAfonso37
        , testProperty "p . kr iden v == kr v p" prop_krAfonso38
        , testProperty "kr iden v == kr v iden" prop_krAfonso39
        , testProperty "k == kr (fstM . k) (sndM . k) for a function k" prop_krReconstruction
        , testProperty "weak product: a column-stochastic k need not be kr (fstM . k) (sndM . k)" prop_krWeakProduct
        ]
    , testGroup "Kronecker"
        [ testProperty "a >< b == kr (a . fstM) (b . sndM)" prop_kronDefinition
        , testProperty "(a >< b) . (c >< d) == (a . c) >< (b . d)" prop_kronFunctor
        , testProperty "iden >< iden == iden" prop_kronIden
        , testProperty "join a b >< c == join (a >< c) (b >< c)" prop_kronFusionJoin
        , testProperty "fork a b >< c == fork (a >< c) (b >< c)" prop_kronFusionFork
        ]
    , testGroup "Direct sum"
        [ testProperty "join a b . (c -|- d) == join (a . c) (b . d)" prop_directSumAbsorption
        , testProperty "(a -|- b) . i1 == i1 . a" prop_directSumI1
        ]
    , testGroup "Abide"
        [ testProperty "abideJF m == m" prop_abideJF
        , testProperty "abideFJ m == m" prop_abideFJ
        , testProperty "abideJF (abideFJ m) == m, relaid out" prop_abideRoundTrip
        , testProperty "exchange: join (fork a c) (fork b d) == fork (join a b) (join c d)" prop_exchange
        , testProperty "join a b .+. join c d == join (a .+. c) (b .+. d)" prop_blockedAddJoin
        , testProperty "fork a b .+. fork c d == fork (a .+. c) (b .+. d)" prop_blockedAddFork
        ]
    , testGroup "Dimension witnesses"
        [ testProperty "sFromNat agrees with FromNat" prop_sFromNat
        , testProperty "Ranged dimOf agrees with FromNat" prop_rangedDimOf
        ]
    , testGroup "Show"
        [ testProperty "show gives the fromLists call" prop_showFromLists
        , testProperty "show does not depend on the layout" prop_showLayout
        ]
    , testGroup "Ord"
        [ testProperty "compare is antisymmetric" prop_ordAntisymmetric
        , testProperty "compare agrees with ==" prop_ordAgreesEq
        ]
    , testGroup "Fixity"
        [ testProperty ".*. binds tighter than .+." prop_fixityHadamard
        , testProperty ".| binds tighter than .+." prop_fixityScalar
        , testProperty "||| binds tighter than ===" prop_fixityBlocks
        ]
    ]

-- Category laws

prop_leftIdentity :: Property
prop_leftIdentity =
  forAll (genSquareMatrix @S5) $ \m ->
    comp iden m == m

prop_rightIdentity :: Property
prop_rightIdentity =
  forAll (genSquareMatrix @S5) $ \m ->
    comp m iden == m

prop_associativity :: Property
prop_associativity =
  forAll ((,,) <$> genSquareMatrix @S3 <*> genSquareMatrix @S3 <*> genSquareMatrix @S3) $
    \(a, b, c) -> approxEqual 1e-6 (comp (comp a b) c) (comp a (comp b c))

-- Composition laws (Macedo and Oliveira 2013, eqs. 35, 27 and 26)

prop_divideAndConquer :: Property
prop_divideAndConquer =
  forAll ((,,,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S5 @S2 <*> genIntMatrix @S5 @S3) $
    \(a, b, c, d) -> comp (join a b) (fork c d) == (comp a c .+. comp b d)

prop_splitFusion :: Property
prop_splitFusion =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S5 <*> genIntMatrix @S2 @S3) $
    \(a, b, c) -> comp (fork a b) c == fork (comp a c) (comp b c)

prop_juncFusion :: Property
prop_juncFusion =
  forAll ((,,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S5 @S3 <*> genIntMatrix @S3 @S2) $
    \(a, b, c) -> comp c (join a b) == join (comp c a) (comp c b)

-- Transpose

prop_trInvolution :: Property
prop_trInvolution =
  forAll (genMatrix @S3 @S5) $ \m ->
    tr (tr m) == m

prop_trContravariant :: Property
prop_trContravariant =
  forAll ((,) <$> genSquareMatrix @S3 <*> genSquareMatrix @S3) $ \(a, b) ->
    approxEqual 1e-6 (tr (comp a b)) (comp (tr b) (tr a))

prop_trIden :: Property
prop_trIden =
  property $ tr (iden :: Matrix Double S5 S5) == (iden :: Matrix Double S5 S5)

-- Biproduct

prop_p1Fork :: Property
prop_p1Fork =
  forAll ((,) <$> genMatrix @(Either S3 S3) @S3 <*> genMatrix @(Either S3 S3) @S3) $ \(a, b) ->
    comp p1 (fork a b) == a

prop_p2Fork :: Property
prop_p2Fork =
  forAll ((,) <$> genMatrix @(Either S3 S3) @S3 <*> genMatrix @(Either S3 S3) @S3) $ \(a, b) ->
    comp p2 (fork a b) == b

prop_joinI1 :: Property
prop_joinI1 =
  forAll ((,) <$> genMatrix @S3 @(Either S3 S3) <*> genMatrix @S3 @(Either S3 S3)) $ \(a, b) ->
    comp (join a b) i1 == a

prop_joinI2 :: Property
prop_joinI2 =
  forAll ((,) <$> genMatrix @S3 @(Either S3 S3) <*> genMatrix @S3 @(Either S3 S3)) $ \(a, b) ->
    comp (join a b) i2 == b

prop_forkUniversal :: Property
prop_forkUniversal =
  forAll (genMatrix @S3 @(Either S3 S3)) $ \m ->
    fork (comp p1 m) (comp p2 m) == m

prop_joinUniversal :: Property
prop_joinUniversal =
  forAll (genMatrix @(Either S3 S3) @S3) $ \m ->
    join (comp m i1) (comp m i2) == m

-- genMatrix lays rows out first; its transpose gives the other layout.
prop_splitJoin :: Property
prop_splitJoin =
  forAll ((,) <$> genMatrix @(Either S3 S2) @S3 <*> genMatrix @S3 @(Either S3 S2)) $ \(m, n) ->
    all (\x -> splitJoin x == (comp x i1, comp x i2)) [m, tr n]

prop_splitFork :: Property
prop_splitFork =
  forAll ((,) <$> genMatrix @S3 @(Either S3 S2) <*> genMatrix @(Either S3 S2) @S3) $ \(m, n) ->
    all (\x -> splitFork x == (comp p1 x, comp p2 x)) [m, tr n]

-- The biproduct equations of Macedo and Oliveira (2013), eqs. 11 to 15, and
-- reflexion (eqs. 18, 19). The first two and the orthogonality laws are also
-- RULES.
prop_biproduct :: Property
prop_biproduct =
  property $
    comp (p1 @Int @S3 @S2) (i1 @Int @S3 @S2) == iden
      && comp (p2 @Int @S3 @S2) (i2 @Int @S3 @S2) == iden

prop_orthogonality :: Property
prop_orthogonality =
  property $
    comp (p1 @Int @S3 @S2) (i2 @Int @S3 @S2) == zeros
      && comp (p2 @Int @S3 @S2) (i1 @Int @S3 @S2) == zeros

prop_biproductSum :: Property
prop_biproductSum =
  property $
    comp (i1 @Int @S3 @S2) p1 .+. comp (i2 @Int @S3 @S2) p2 == iden

prop_reflexion :: Property
prop_reflexion =
  property $
    join (i1 @Int @S3 @S2) i2 == iden && fork (p1 @Int @S3 @S2) p2 == iden

-- Bilinearity

prop_bilinearLeft :: Property
prop_bilinearLeft =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S4 @S3 <*> genIntMatrix @S4 @S3) $ \(m, n, l) ->
    comp m (n .+. l) == comp m n .+. comp m l

prop_bilinearRight :: Property
prop_bilinearRight =
  forAll ((,,) <$> genIntMatrix @S4 @S3 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S2) $ \(m, n, l) ->
    comp (n .+. l) m == comp n m .+. comp l m

-- Element-wise

prop_addCommutative :: Property
prop_addCommutative =
  forAll ((,) <$> genSquareMatrix @S5 <*> genSquareMatrix @S5) $ \(a, b) ->
    (a .+. b) == (b .+. a)

prop_addIdentity :: Property
prop_addIdentity =
  forAll (genSquareMatrix @S5) $ \m ->
    m .+. zeros == m

prop_hadCommutative :: Property
prop_hadCommutative =
  forAll ((,) <$> genSquareMatrix @S5 <*> genSquareMatrix @S5) $ \(a, b) ->
    (a .*. b) == (b .*. a)

prop_hadIdentity :: Property
prop_hadIdentity =
  forAll (genSquareMatrix @S5) $ \m ->
    m .*. ones == m

-- Scalar

prop_scalarIdentity :: Property
prop_scalarIdentity =
  forAll (genSquareMatrix @S5) $ \m ->
    (1 :: Double) .| m == m

prop_scalarDistributive :: Property
prop_scalarDistributive =
  forAll ((,) <$> genSquareMatrix @S5 <*> genSquareMatrix @S5) $ \(a, b) ->
    approxEqual 1e-6 (3.14 .| (a .+. b)) ((3.14 .| a) .+. (3.14 .| b))

-- Construction

prop_fromToLists :: Property
prop_fromToLists =
  forAll (genSquareMatrix @S5) $ \m ->
    (fromLists (toLists m) :: Matrix Double S5 S5) == m

prop_fromFId :: Property
prop_fromFId =
  property $
    (fromF id :: Matrix Double S5 S5) == iden

prop_fromFComp :: Property
prop_fromFComp =
  property $
    approxEqual 1e-10
      (comp (fromF not :: Matrix Double Bool Bool) (fromF not :: Matrix Double Bool Bool))
      (fromF (not . not) :: Matrix Double Bool Bool)

-- Khatri-Rao

prop_krScale :: Property
prop_krScale =
  forAll ((,) <$> genMatrix @S2 @() <*> genMatrix @S2 @()) $ \(f, g) ->
    approxEqual 1e-6 (kr (2 .| f) g) (2 .| kr f g)

-- Macedo and Oliveira (2013, sec. 13): the Khatri-Rao product from Hadamard.
prop_krDefinition :: Property
prop_krDefinition =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $ \(a, b) ->
    kr a b == (comp (tr (fstM @Int @S2 @S4)) a .*. comp (tr sndM) b)

-- The projections return each factor scaled by the column sums of the other.
prop_krFstM :: Property
prop_krFstM =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $ \(a, b) ->
    comp (fstM @Int @S2 @S4) (kr a b) == a .*. comp ones b

prop_krSndM :: Property
prop_krSndM =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $ \(a, b) ->
    comp (sndM @Int @S2 @S4) (kr a b) == comp ones a .*. b

-- The unit of kr is bang. The row index type is ((), b) rather than b, so
-- compare the entries.
prop_krUnit :: Property
prop_krUnit =
  forAll (genIntMatrix @S3 @S4) $ \a ->
    toLists (kr bang a) == toLists a && toLists (kr a bang) == toLists a

-- For column-stochastic matrices Khatri-Rao is a weak product (Murta and
-- Oliveira 2013, eq. 22), so projecting kr a b gives a and b back.
prop_krCancel :: Property
prop_krCancel =
  forAll ((,) <$> genStochastic @S2 @S2 <*> genStochastic @S2 @S3) $ \(a, b) ->
    let p = kr a b
     in approxEqual 1e-9 (comp fstM p) a && approxEqual 1e-9 (comp sndM p) b

-- Cancellation needs every column of the other factor to sum to 1, which
-- functions and column-stochastic matrices satisfy.
prop_krCancelFunctions :: Property
prop_krCancelFunctions =
  forAll ((,) <$> (fromF <$> arbitrary @(S3 -> S2)) <*> (fromF <$> arbitrary @(S3 -> S4))) $ \(a, b) ->
    comp (fstM @Int) (kr a b) == a && comp (sndM @Int) (kr a b) == b

prop_krCancelFst :: Property
prop_krCancelFst =
  forAll ((,) <$> genMatrix @S3 @S2 <*> genStochastic @S3 @S4) $ \(a, b) ->
    approxEqual 1e-9 (comp fstM (kr a b)) a

prop_krCancelSnd :: Property
prop_krCancelSnd =
  forAll ((,) <$> genStochastic @S3 @S2 <*> genMatrix @S3 @S4) $ \(a, b) ->
    approxEqual 1e-9 (comp sndM (kr a b)) b

-- Macedo and Oliveira (2013, sec. 13) define fstM as iden >< bang and sndM as
-- bang >< iden.
prop_krProjections :: Property
prop_krProjections =
  property $
    toLists (fstM @Int @S2 @S3) == toLists (iden @Int @S2 >< bang @Int @S3)
      && toLists (sndM @Int @S2 @S3) == toLists (bang @Int @S2 >< iden @Int @S3)

-- Khatri-Rao laws. Each holds for arbitrary matrices unless its test says
-- otherwise. Where the two sides have different index
-- types but the same dimensions, the underlying matrices are compared.

-- Macedo and Oliveira (2013, sec. 13); Macedo and Oliveira (2015, OLAP, sec. 5).
prop_krAssoc :: Property
prop_krAssoc =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4 <*> genIntMatrix @S3 @S2) $ \(a, b, c) ->
    let M l = kr (kr a b) c
        M r = kr a (kr b c)
     in l == r

-- Macedo and Oliveira (2015, OLAP, eq. 13); Murta and Oliveira (2013, eq. 16).
prop_krBlocks :: Property
prop_krBlocks =
  forAll ((,,,) <$> genIntMatrix @S2 @S2 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S2 @S4 <*> genIntMatrix @S3 @S4) $
    \(m, n, p, q) ->
      let M l = kr (join m n) (join p q)
          M r = join (kr m p) (kr n q)
       in l == r

-- Macedo and Oliveira (2015, OLAP, eq. 15).
prop_krRowFork :: Property
prop_krRowFork =
  forAll ((,,) <$> genIntMatrix @S3 @() <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $ \(v, m, n) ->
    let M l = kr v (fork m n)
        M r = fork (kr v m) (kr v n)
     in l == r

-- Macedo and Oliveira (2015, OLAP, eq. 13); Murta and Oliveira (2013, eq. 15).
prop_krColumns :: Property
prop_krColumns =
  forAll ((,) <$> genIntMatrix @() @S2 <*> genIntMatrix @() @S4) $ \(u, v) ->
    let M l = kr u v
        M r = u >< v
     in l == r

-- Afonso et al. (2018, eq. 10).
prop_krRows :: Property
prop_krRows =
  forAll ((,) <$> genIntMatrix @S3 @() <*> genIntMatrix @S3 @()) $ \(v, u) ->
    let M l = kr v u
        M r = v .*. u
     in l == r

-- Macedo and Oliveira (2015, OLAP, eq. 14).
prop_krDiagonal :: Property
prop_krDiagonal =
  forAll (genIntMatrix @S3 @()) $ \s ->
    let xs = concat (toLists s)
     in toLists (kr s (iden @Int @S3)) == [[if i == j then x else 0 | j <- [0 .. length xs - 1]] | (i, x) <- zip [0 ..] xs]

-- Rao (1970), the mixed product with Kronecker.
prop_krMixedProduct :: Property
prop_krMixedProduct =
  forAll ((,,,) <$> genIntMatrix @S2 @S4 <*> genIntMatrix @S4 @S2 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $
    \(a, b, c, d) -> comp (a >< b) (kr c d) == kr (comp a c) (comp b d)

-- Rao (1970), the Gram rule.
prop_krGram :: Property
prop_krGram =
  forAll ((,,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4) $
    \(a, b, c, d) -> comp (tr (kr a b)) (kr c d) == (comp (tr a) c .*. comp (tr b) d)

prop_krBilinear :: Property
prop_krBilinear =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4 <*> genIntMatrix @S3 @S4) $ \(a, b, c) ->
    kr a (b .+. c) == (kr a b .+. kr a c)

-- Murta and Oliveira (2013, eq. A.1): fusion needs h to be a function.
prop_krFusion :: Property
prop_krFusion =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4 <*> (fromF <$> arbitrary @(S2 -> S3))) $ \(m, n, h) ->
    comp (kr m n) h == kr (comp m h) (comp n h)

-- Afonso et al. (2018, eqs. 36 to 39), with v a row vector.
prop_krAfonso36 :: Property
prop_krAfonso36 =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S4 <*> genIntMatrix @S3 @()) $ \(p, q, v) ->
    let M l = comp p (tr (kr q v))
        M r = comp (kr v p) (tr q)
     in l == r

prop_krAfonso37 :: Property
prop_krAfonso37 =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @()) $ \(p, v) ->
    let M l = comp p (tr v)
        M r = comp (kr v p) (tr bang)
     in l == r

-- p is typed S3 -> S2 while kr iden v lands in (S3, ()), so the composition is
-- taken on the underlying matrices.
prop_krAfonso38 :: Property
prop_krAfonso38 =
  forAll ((,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @()) $ \(p, v) ->
    let M p' = p
        M kv = kr (iden @Int @S3) v
        M r = kr v p
     in I.comp p' kv == r

prop_krAfonso39 :: Property
prop_krAfonso39 =
  forAll (genIntMatrix @S3 @()) $ \v ->
    let M l = kr (iden @Int @S3) v
        M r = kr v (iden @Int @S3)
     in l == r

-- Murta and Oliveira (2013, eq. 19): reconstruction holds for functions.
prop_krReconstruction :: Property
prop_krReconstruction =
  forAll (fromF <$> arbitrary @(S3 -> (S2, S4))) $ \k ->
    k == kr (comp fstM k) (comp (sndM @Int) k)

-- Murta and Oliveira (2013, sec. 6): their column-stochastic k is not rebuilt
-- by pairing its projections, although cancellation holds on the pair, so
-- cancellation is only an implication.
prop_krWeakProduct :: Property
prop_krWeakProduct =
  property $
    let k =
          fromLists
            [ [0, 0.4, 0.2]
            , [0.2, 0, 0.17]
            , [0.2, 0.1, 0.13]
            , [0.6, 0.4, 0.2]
            , [0, 0, 0.17]
            , [0, 0.1, 0.13]
            ] ::
            Matrix Double (Ranged 1 3) (Ranged 1 2, Ranged 1 3)
        k' = kr (comp fstM k) (comp sndM k)
     in not (approxEqual 1e-9 k k')
          && approxEqual 1e-9 (comp fstM k') (comp fstM k)
          && approxEqual 1e-9 (comp sndM k') (comp sndM k)

-- Kronecker

-- Macedo and Oliveira (2013, sec. 13): Kronecker from Khatri-Rao.
prop_kronDefinition :: Property
prop_kronDefinition =
  forAll ((,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S2) $ \(a, b) ->
    (a >< b) == kr (comp a fstM) (comp b sndM)

-- Macedo and Oliveira (2013), eq. 54.
prop_kronFunctor :: Property
prop_kronFunctor =
  forAll ((,,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S2 @S3 <*> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S2) $
    \(a, b, c, d) -> comp (a >< b) (c >< d) == (comp a c >< comp b d)

-- Macedo and Oliveira (2013), eq. 55.
prop_kronIden :: Property
prop_kronIden =
  property $ (iden @Int @S2 >< iden @Int @S3) == iden

-- Macedo and Oliveira (2013), eqs. 60 and 61. The index types of the two sides differ ((Either a b, c)
-- against Either (a, c) (b, c)) but their dimensions are the same, so compare
-- the underlying matrices.
prop_kronFusionJoin :: Property
prop_kronFusionJoin =
  forAll ((,,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S2 @S2) $ \(a, b, c) ->
    let M l = join a b >< c
        M r = join (a >< c) (b >< c)
     in l == r

prop_kronFusionFork :: Property
prop_kronFusionFork =
  forAll ((,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S2 @S2) $ \(a, b, c) ->
    let M l = fork a b >< c
        M r = fork (a >< c) (b >< c)
     in l == r

-- Direct sum (Macedo and Oliveira 2013, eqs. 64, 65)

prop_directSumAbsorption :: Property
prop_directSumAbsorption =
  forAll ((,,,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S4 @S2 <*> genIntMatrix @S2 @S3) $
    \(a, b, c, d) -> comp (join a b) (c -|- d) == join (comp a c) (comp b d)

prop_directSumI1 :: Property
prop_directSumI1 =
  forAll ((,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S4 @S2) $ \(a, b) ->
    comp (a -|- b) i1 == comp i1 a

-- Abide

prop_abideJF :: Property
prop_abideJF =
  forAll (genMatrix @(Either S3 S3) @(Either S3 S3)) $ \m ->
    abideJF m == m

prop_abideFJ :: Property
prop_abideFJ =
  forAll (genMatrix @(Either S3 S3) @(Either S3 S3)) $ \m ->
    abideFJ m == m

-- genMatrix never puts a Fork under a Join, so abideJF m leaves m as it is.
-- abideFJ creates such nodes first (the Show check confirms the layout
-- changed), which makes abideJF use its exchange clause.
prop_abideRoundTrip :: Property
prop_abideRoundTrip =
  forAll (genMatrix @(Either S3 S3) @(Either S3 S3)) $ \m ->
    let m' = abideFJ m
        layout (M x) = show x
     in layout m' /= layout m && abideJF m' == m

-- Macedo and Oliveira (2013), eq. 30.
prop_exchange :: Property
prop_exchange =
  forAll ((,,,) <$> genIntMatrix @S2 @S2 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3) $
    \(a, b, c, d) -> join (fork a c) (fork b d) == fork (join a b) (join c d)

-- Macedo and Oliveira (2013), eqs. 31 and 32.
prop_blockedAddJoin :: Property
prop_blockedAddJoin =
  forAll ((,,,) <$> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S2 @S3 <*> genIntMatrix @S3 @S3) $
    \(a, b, c, d) -> (join a b .+. join c d) == join (a .+. c) (b .+. d)

prop_blockedAddFork :: Property
prop_blockedAddFork =
  forAll ((,,,) <$> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S3 <*> genIntMatrix @S3 @S2 <*> genIntMatrix @S3 @S3) $
    \(a, b, c, d) -> (fork a b .+. fork c d) == fork (a .+. c) (b .+. d)

-- Fixity

prop_fixityHadamard :: Property
prop_fixityHadamard =
  property $
    toLists (ones .+. ones .*. zeros :: Matrix Double Bool Bool) == [[1, 1], [1, 1]]

prop_fixityScalar :: Property
prop_fixityScalar =
  property $
    toLists (2 .| ones .+. ones :: Matrix Double Bool Bool) == [[3, 3], [3, 3]]

prop_fixityBlocks :: Property
prop_fixityBlocks =
  property $
    toLists (one 1 ||| one 2 === one 3 ||| one (4 :: Double)) == [[1, 2], [3, 4]]

-- Dimension witnesses

-- The Nat kernel builds FromNat trees at runtime, so it has to agree with the
-- type family for every size.
fromNatAgrees :: forall n. (KnownNat n, I.KnownDim (I.FromNat n)) => Bool
fromNatAgrees = isJust (I.eqSDim (I.sFromNat @n) (I.dimSing @(I.FromNat n)))

prop_sFromNat :: Property
prop_sFromNat =
  property $
    and
      [ fromNatAgrees @1, fromNatAgrees @2, fromNatAgrees @3, fromNatAgrees @4
      , fromNatAgrees @5, fromNatAgrees @6, fromNatAgrees @7, fromNatAgrees @8
      , fromNatAgrees @9, fromNatAgrees @10, fromNatAgrees @11, fromNatAgrees @12
      , fromNatAgrees @13, fromNatAgrees @14, fromNatAgrees @15, fromNatAgrees @16
      , fromNatAgrees @17, fromNatAgrees @31, fromNatAgrees @32, fromNatAgrees @33
      , fromNatAgrees @64, fromNatAgrees @100, fromNatAgrees @125
      , fromNatAgrees @1000, fromNatAgrees @1001
      ]

prop_rangedDimOf :: Property
prop_rangedDimOf =
  property $
    and
      [ isJust (I.eqSDim (dimOf @(Ranged 0 9)) (I.dimSing @(I.FromNat 10)))
      , isJust (I.eqSDim (dimOf @(Ranged 3 12)) (I.dimSing @(I.FromNat 10)))
      , isJust (I.eqSDim (dimOf @(Ranged 1 6)) (I.dimSing @(I.FromNat 6)))
      , cardinality @(Ranged 2 12) == 11
      ]

-- Ord

prop_ordAntisymmetric :: Property
prop_ordAntisymmetric =
  forAll ((,) <$> genIntMatrix @S3 @S3 <*> genIntMatrix @S3 @S3) $ \(a, b) ->
    compare a b == invert (compare b a)
  where
    invert LT = GT
    invert GT = LT
    invert EQ = EQ

prop_ordAgreesEq :: Property
prop_ordAgreesEq =
  forAll ((,) <$> genIntMatrix @S2 @S2 <*> genIntMatrix @S2 @S2) $ \(a, b) ->
    (a == b) == (compare a b == EQ) && compare a a == EQ

-- Show

prop_showFromLists :: Property
prop_showFromLists =
  forAll (genMatrix @S3 @(Either S2 S2)) $ \m -> show m == "fromLists " ++ show (toLists m)

prop_showLayout :: Property
prop_showLayout =
  forAll (genMatrix @(Either S2 S3) @(Either S3 S2)) $ \m -> show (abideFJ m) == show m
