{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module LAoP.Dist.Internal
        ( 
        Dist(..),
        Prob,

        fmapD,
        unitD,
        multD,
        selectD,
        returnD,
        bindD,
        (??),
        
        choose,
        shape,
        linear,
        uniform,
        negExp,
        normal,
        toValues,
        prettyDist,
        prettyPrintDist
        )
    where

import LAoP.Matrix.Type
import LAoP.Utils
import GHC.TypeLits
import Data.Proxy
import Data.List (sortBy)

-- | Type synonym for probability value
type Prob = Double

-- | Type synonym for column vector matrices. This represents a probability
-- distribution.
newtype Dist a = D (Matrix Prob () a)

-- | Functor instance
fmapD :: 
     ( Bounded a,
       Bounded b,
       Enum a,
       Enum b,
       Eq b,
       KnownNat (Count (Normalize a)),
       KnownNat (Count (Normalize b)),
       FromLists Prob (Normalize b) (Normalize a)
     )
     =>
     (a -> b) -> Dist a -> Dist b
fmapD f (D m) = D (fromF' f `comp` m)

-- | Applicative/Monoidal instance 'unit' function
unitD :: Dist ()
unitD = D (one 1)

-- | Applicative/Monoidal instance 'mult' function
multD :: 
      ( KnownNat (Count (Normalize a)),
        KnownNat (Count (Normalize b)),
        KnownNat (Count (Normalize (a, b))),
        FromLists Prob (Normalize (a, b)) (Normalize a),
        FromLists Prob (Normalize (a, b)) (Normalize b),
        Normalize (a, b) ~ Normalize (Normalize a, Normalize b)
      ) => Dist a -> Dist b -> Dist (a, b)
multD (D a) (D b) = D (khatri a b)

-- | Selective instance function
selectD :: 
       ( Normalize (Either a b) ~ Either (Normalize a) (Normalize b),
         FromLists Prob (Normalize b) (Normalize b),
         KnownNat (Count (Normalize b))
       ) => Dist (Either a b) -> Matrix Prob a b -> Dist b
selectD (D d) m = D (junc m identity `comp` d)


-- | Monad instance 'return' function
returnD :: forall a . (Enum a, FromLists Prob () (Normalize a), KnownNat (Count a)) => a -> Dist a
returnD a = D (col l)
    where
        i = fromInteger $ natVal (Proxy :: Proxy (Count a))
        x = fromEnum a
        l = take x [0,0..] ++ [1] ++ take (i - x - 1) [0,0..]

-- | Monad instance '(>>=)' function
bindD :: Dist a -> Matrix Prob a b -> Dist b
bindD (D d) m = D (m `comp` d)

-- | Extract probabilities given an Event.
(??) :: 
     ( Enum a, 
       KnownNat (Count a), 
       FromLists Prob () (Normalize a)
     ) => (a -> Bool) -> Dist a -> Prob
(??) p d =
    let l = toValues d
        x = filter (p . fst) l
     in sum . map snd $ x

-- Distribution Construction

-- | Constructs a Bernoulli distribution
choose :: (FromLists Prob () (Normalize a)) => Prob -> Dist a
choose prob = D (col [prob, 1 - prob])

-- | Creates a distribution given a shape function
shape :: (FromLists Prob () (Normalize a)) => (Prob -> Prob) -> [a] -> Dist a
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = map f (iterate (+incr) 0)
   in  fromFreqs (zip xs ps)

-- | Constructs a Linear distribution
linear :: (FromLists Prob () (Normalize a)) => [a] -> Dist a
linear = shape id

-- | Constructs an Uniform distribution
uniform :: (FromLists Prob () (Normalize a)) => [a] -> Dist a
uniform = shape (const 1)

-- | Constructs an Negative Exponential distribution
negExp :: (FromLists Prob () (Normalize a)) => [a] -> Dist a
negExp = shape (\x -> exp (-x))

-- | Constructs an Normal distribution
normal :: (FromLists Prob () (Normalize a)) => [a] -> Dist a
normal = shape (normalCurve 0.5 0.5)

-- | Transforms a 'Dist' into a list of pairs.
toValues :: forall a . (Enum a, KnownNat (Count a), FromLists Prob () (Normalize a)) => Dist a -> [(a, Prob)]
toValues (D d) =
    let rows = fromInteger (natVal (Proxy :: Proxy (Count a)))
        probs = toList d
        res = zip (map toEnum [0..rows]) probs
     in res

-- | Pretty a distribution
prettyDist :: forall a. (Show a, Enum a, KnownNat (Count a), FromLists Prob () (Normalize a)) => Dist a -> String
prettyDist d =
    let values = sortBy (\(a, p1) (b, p2) -> compare p2 p1) (toValues @a d)
        w = maximum (map (length . show . fst) values)
     in concatMap
          (\(x,p) -> showR w x ++ ' ': showProb p ++ "\n")
          values
  where
    showProb p = show (p * 100) ++ "%"
    showR n x = show x ++ " " 

-- | Pretty Print a distribution
prettyPrintDist :: forall a . (Show a, Enum a, KnownNat (Count a), FromLists Prob () (Normalize a)) => Dist a -> IO ()
prettyPrintDist = putStrLn . prettyDist @a

-- Auxiliary functions

fromFreqs :: (FromLists Prob () (Normalize a)) => [(a,Prob)] -> Dist a
fromFreqs xs = D (col (map (\(x,p) -> p/q) xs))
           where q = sum $ map snd xs

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
