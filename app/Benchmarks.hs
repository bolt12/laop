{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Benchmarks
    ( benchmark
    )
  where

import LAoP.Dist.Internal
import LAoP.Utils
import LAoP.Matrix.Type hiding (CountableN, Countable, FromListsN)
import qualified LAoP.Matrix.Type as T (FromListsN)
import Criterion.Main
import Control.DeepSeq
import Test.QuickCheck
import GHC.TypeLits
import Data.Proxy

selectM2 :: 
       ( Num e,
         T.FromListsN e b b,
         CountableN b
       ) => Matrix e cols (Either a b) -> Matrix e a b -> Matrix e cols b
selectM2 m y = junc y identity `comp` m

selectD2 :: 
       ( FromListsN b b,
         CountableN b
       ) => Dist (Either a b) -> Matrix Prob a b -> Dist b
selectD2 (D d) m = D (junc m identity `comp` d)

randomDist :: forall a . (Countable a, FromListsN () a) => Gen (Dist a)
randomDist = do
  let size = fromInteger (natVal (Proxy :: Proxy (Count a)))
  l <- vectorOf size (arbitrary :: Gen Prob)
  let ln = normalize l
      lr = map (:[]) ln
      m  = fromLists lr
  return (D m)

randomMatrix :: forall a b . (CountableDimensions a b, FromListsN a b) => Gen (Matrix Prob a b)
randomMatrix = do
  let cols = fromInteger (natVal (Proxy :: Proxy (Count a)))
      rows = fromInteger (natVal (Proxy :: Proxy (Count b)))
  l <- vectorOf (cols * rows) arbitrary
  let lr = buildList l cols
      m  = fromLists lr
  return m

buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

normalize :: [Prob] -> [Prob]
normalize l = let l' = map abs l
                  s  = sum l'
               in map (/ s) l'

-- Probability
newtype Probability = P Double
  deriving (Num, Show, Fractional, NFData) via Double
newtype Dist' a = D' { unD :: [(a,Probability)] }
  deriving (Show, NFData)

instance Functor Dist' where
  fmap f (D' d) = D' [(f x,p) | (x,p) <- d]

instance Applicative Dist' where
  pure x = D' [(x, 1)]
  fm <*> m = D' [(f x,q*p) | (f,p) <- unD fm, (x,q) <- unD m]

class Applicative f => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b

instance Selective Dist' where
  select x y = (\e f -> either f id e) <$> x <*> y -- selectA

instance Monad Dist' where
  return       = pure
  (D' d) >>= f = D' [(y,q*p) | (x,p) <- d, (y,q) <- unD (f x)]

normalize2 :: forall a . (Enum a, Bounded a) => [Probability] -> [(a, Probability)]
normalize2 l = let l' = map abs l
                   as = [minBound .. maxBound]
                   s  = sum l'
                   probs = map (/ s) l'
                in zip as probs

randomDist2 :: forall a . (Countable a, Enum a, Bounded a) => Gen (Dist' a)
randomDist2 = do
  let size = fromInteger (natVal (Proxy :: Proxy (Count a)))
  l <- vectorOf size (arbitrary :: Gen Prob)
  let ln = normalize2 @a (map P l)
  return (D' ln)

normalize3 :: forall a . [a] -> [Probability] -> [(a, Probability)]
normalize3 as l = let s  = sum l
                      probs = map (/ s) l
                   in zip as probs

randomDistF :: forall a b . (CoArbitrary a, Arbitrary b) => Gen (Dist' (a -> b))
randomDistF = do
  let size = 200
  l <- vectorOf size (arbitrary :: Gen Prob)
  l2 <- vectorOf size (arbitrary :: Gen (a -> b))
  let ln = normalize3 l2 (map P l)
  return (D' ln)

instance CoArbitrary (Natural a b)

instance forall (a :: Nat) (b :: Nat). (KnownNat a, KnownNat b) => Arbitrary (Natural a b)
  where
  arbitrary =
    let bottom = fromInteger (natVal (Proxy :: Proxy a))
        top    = fromInteger (natVal (Proxy :: Proxy b))
     in do
       x <- arbitrary `suchThat` (\n -> n >= bottom && n <= top)
       return (nat x)

setupEnv = do
  m11 <- generate (resize 1 (randomMatrix @(Natural 0 10) @(Natural 0 10) ))
  m12 <- generate (resize 1 (randomMatrix @(Natural 0 10) @(Natural 0 10) ))
  m21 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Natural 0 100) ))
  m22 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Natural 0 100) ))
  m31 <- generate (resize 1 (randomMatrix @(Natural 0 200) @(Natural 0 200) ))
  m32 <- generate (resize 1 (randomMatrix @(Natural 0 200) @(Natural 0 200) ))
  return (m11, m12, m21, m22, m31, m32)

setupEnv2 = do
  m21 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Natural 0 100) ))
  m40 <- generate (resize 1 (randomMatrix @(Natural 0 300) @(Either (Natural 0 100) (Natural 0 100) )))
  dist <- generate (resize 1 (randomDist @(Either (Natural 0 1000) (Natural 0 1000)) ))
  dist2 <- generate (resize 1 (randomMatrix @(Natural 0 1000) @(Natural 0 1000) ))
  distList1 <- generate (resize 1 (randomDist2 @(Either (Natural 0 1000) (Natural 0 1000))))
  distList2 <- generate (resize 1 (randomDistF @(Natural 0 1000) @(Natural 0 1000)))
  return (m21, m40, dist, dist2, distList1, distList2)

benchmark :: IO ()
benchmark = defaultMain [
   env setupEnv $ \ ~(m11, m12, m21, m22, m31, m32) -> bgroup "Matrix Composition" [
   bgroup "10x10" [
     bench "WHNF - 10x10" $ whnf (comp m11) m12
   , bench "NF - 10x10" $ nf (comp m11) m12
   ],
   bgroup "100x100" [
     bench "WHNF - 100x100" $ whnf (comp m21) m22
   , bench "NF - 100x100" $ nf (comp m21) m22
   ],
   bgroup "200x200" [
     bench "WHNF - 200x200" $ whnf (comp m31) m32
   , bench "NF - 200x200" $ nf (comp m31) m32
   ] ],
   env setupEnv2 $ \ ~(m21, m40, dist, dist2, dl1, dl2) -> bgroup "Matrix vs List - `select`" [
   bgroup "Distribution" [
     bench "List Distribution - Applicative version" $ nf (select dl1) dl2
   , bench "Matrix Distribution - Applicative version" $ nf (selectD2 dist) dist2
   , bench "Matrix Distribution - Selective version" $ nf (selectD dist) dist2
   ],
   bgroup "Matrix `select`" [
     bench "Applicative version" $ nf (selectM2 m40) m21
   , bench "Selective version" $ nf (selectM m40) m21
   ] ] ]
