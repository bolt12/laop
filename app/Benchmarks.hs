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
import Data.Time.Clock.POSIX (getPOSIXTime)

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
  l <- vectorOf (cols * rows) (arbitrary :: Gen Float)
  let lr = buildList l cols
      m  = fromLists lr
  return m

buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

normalize :: [Prob] -> [Prob]
normalize l = let l' = map abs l
                  s  = sum l'
               in map (/ s) l'

setupEnv = do
  m11 <- generate (resize 1 (randomMatrix @(Natural 0 10) @(Natural 0 10) ))
  m12 <- generate (resize 1 (randomMatrix @(Natural 0 10) @(Natural 0 10) ))
  m21 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Natural 0 100) ))
  m22 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Natural 0 100) ))
  m31 <- generate (resize 1 (randomMatrix @(Natural 0 300) @(Natural 0 300) ))
  m32 <- generate (resize 1 (randomMatrix @(Natural 0 300) @(Natural 0 300) ))
  m40 <- generate (resize 1 (randomMatrix @(Natural 0 100) @(Either (Natural 0 100) (Natural 0 100) )))
  dist <- generate (resize 1 (randomDist @(Either (Natural 0 1000) (Natural 0 1000)) ))
  dist2 <- generate (resize 1 (randomMatrix @(Natural 0 1000) @(Natural 0 1000) ))
  return (m11, m12, m21, m22, m31, m32, m40, dist, dist2)

benchmark :: IO ()
benchmark = defaultMain [
   env setupEnv $ \ ~(m11, m12, m21, m22, m31, m32, m40, dist, dist2) -> bgroup "main" [
   bgroup "Matrix composition" [
     bench "WHNF - 10x10" $ whnf (comp m11) m12
   , bench "NF - 10x10" $ nf (comp m11) m12
   , bench "WHNF - 100x100" $ whnf (comp m21) m22
   , bench "NF - 100x100" $ nf (comp m21) m22
   , bench "WHNF - 300x300" $ whnf (comp m31) m32
   , bench "NF - 300x300" $ nf (comp m31) m32
   ],
   bgroup "Dist - Applicative `select`" [
      bench "WHNF" $ whnf (selectD2 dist) dist2
   ,  bench "NF" $ nf (selectD2 dist) dist2
   ],
   bgroup "Dist - Selective `select`" [
     bench "WHNF" $ whnf (selectD dist) dist2
   , bench "NF" $ nf (selectD dist) dist2
   ],
   bgroup "Matrix - Applicative `select`" [
      bench "WHNF" $ whnf (selectM2 m40) m21
   ,  bench "NF" $ nf (selectM2 m40) m21
   ],
   bgroup "Matrix - Selective `select`" [
     bench "WHNF" $ whnf (selectM m40) m21
   , bench "NF" $ nf (selectM m40) m21
   ]
 ] ]
