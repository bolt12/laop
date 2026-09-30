{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
-- Arbitrary instances for library types live here, not in the library, so the
-- library does not depend on QuickCheck.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Generators (
  genMatrix,
  genSquareMatrix,
  genIntMatrix,
  genRelation,
  genDist,
  genStochastic,
  matricesEqual,
  approxEqual,
  errorOf,
  type S2,
  type S3,
  type S4,
  type S5,
  type S10,
) where

import           Control.Exception      (ErrorCall (..), evaluate, try)
import           Data.List              (transpose)
import           Data.Proxy
import           GHC.TypeLits
import           LAoP.Dist.Internal     (Dist (..), Prob)
import           LAoP.Matrix.Indexed
import           LAoP.Relation.Internal (Relation (..))
import qualified LAoP.Relation.Internal as R
import           LAoP.Utils
import           Prelude                hiding (id, (.))
import           Test.QuickCheck

type S2  = Ranged 0 1
type S3  = Ranged 0 2
type S4  = Ranged 0 3
type S5  = Ranged 0 4
type S10 = Ranged 0 9

instance CoArbitrary (Ranged a b) where
  coarbitrary (Rng i) = coarbitrary i

instance forall (a :: Nat) (b :: Nat). (KnownNat a, KnownNat b) => Arbitrary (Ranged a b) where
  arbitrary =
    let bottom = fromInteger (natVal (Proxy :: Proxy a))
        top = fromInteger (natVal (Proxy :: Proxy b))
     in do
          x <- choose (bottom, top)
          return (mkRanged x)

genMatrix ::
  forall a b.
  ( MatIndex a
  , MatIndex b
  ) =>
  Gen (Matrix Double a b)
genMatrix = do
  let c = cardinality @a
      r = cardinality @b
  l <- vectorOf (c * r) arbitrary
  let lr = buildList l c
  return (fromLists lr)

genSquareMatrix ::
  forall a.
  ( MatIndex a
  ) =>
  Gen (Matrix Double a a)
genSquareMatrix = genMatrix @a @a

genIntMatrix ::
  forall a b.
  ( MatIndex a
  , MatIndex b
  ) =>
  Gen (Matrix Int a b)
genIntMatrix = do
  let c = cardinality @a
      r = cardinality @b
  l <- vectorOf (c * r) (choose (-100, 100))
  let lr = buildList l c
  return (fromLists lr)

genRelation ::
  forall a b.
  ( MatIndex a
  , MatIndex b
  ) =>
  Gen (Relation a b)
genRelation = do
  let c = cardinality @a
      r = cardinality @b
  l <- vectorOf (c * r) (elements [0, 1])
  let lr = buildList l c
  return (R.fromLists lr)

genDist ::
  forall a.
  ( MatIndex a
  ) =>
  Gen (Dist a)
genDist = do
  let size = cardinality @a
  l <- vectorOf size (choose (0.01, 100) :: Gen Prob)
  let s = sum l
      ln = map (\x -> [x / s]) l
  return (D (fromLists ln))

-- A column-stochastic matrix: every column is a distribution over b.
genStochastic ::
  forall a b.
  ( MatIndex a
  , MatIndex b
  ) =>
  Gen (Matrix Prob a b)
genStochastic = do
  let c = cardinality @a
      r = cardinality @b
  cols <- vectorOf c $ do
    ws <- vectorOf r (choose (0.01, 100) :: Gen Prob)
    let s = sum ws
    return (map (/ s) ws)
  return (fromLists (transpose cols))

buildList :: [a] -> Int -> [[a]]
buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

matricesEqual :: (Eq e) => Matrix e a b -> Matrix e a b -> Bool
matricesEqual a b = toLists a == toLists b

approxEqual :: Double -> Matrix Double a b -> Matrix Double a b -> Bool
approxEqual tol a b =
  all (\(x, y) -> abs (x - y) < tol) (zip (concat (toLists a)) (concat (toLists b)))

-- Evaluates to WHNF and returns the error message, if any.
errorOf :: a -> IO (Maybe String)
errorOf x = either (\(ErrorCall msg) -> Just msg) (const Nothing) <$> try (evaluate x)
