{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
Module     : LAoP.Dist.Internal
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Probability distributions represented as column vectors whose entries are
non-negative and sum to 1, and probabilistic functions as column-stochastic
matrices, whose columns are such vectors (Oliveira 2012).

This module exports the 'D' constructor, which "LAoP.Dist" hides: a vector
given to it is trusted to be a distribution.
-}
module LAoP.Dist.Internal (
  Dist (..),
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
  fromFreqs,
  toValues,
  toMatrix,
  prettyDist,
  prettyPrintDist,
) where

import           Control.DeepSeq
import           Data.Array           (accumArray, (!))
import           Data.List            (sortBy)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           GHC.Stack            (HasCallStack)
import qualified LAoP.Matrix.Indexed  as IX
import qualified LAoP.Matrix.Internal as I

-- | Probability values.
type Prob = Double

{- | A probability distribution over @a@: a column vector from
"LAoP.Matrix.Indexed" whose entries are non-negative and sum to 1.

"LAoP.Dist" exports the type without its constructor, so every distribution
comes from the functions below. The constructor 'D' skips that check: a vector
given to it directly is trusted to be a distribution.
-}
newtype Dist a = D (IX.Matrix Prob () a)
  deriving (Eq, Ord, NFData) via (IX.Matrix Prob () a)

-- | Shows a distribution as the 'fromFreqs' call that builds it.
instance (IX.MatIndex a, Show a) => Show (Dist a) where
  showsPrec d dist = showParen (d > 10) (showString "fromFreqs " . showsPrec 11 (toValues dist))

-- | The column vector of a distribution.
toMatrix :: Dist a -> IX.Matrix Prob () a
toMatrix (D m) = m

{- | Functor instance. A function is a matrix, so mapping it over a
distribution is a product:

@
'fmapD' f d == 'D' ('IX.comp' ('IX.fromF' f) ('toMatrix' d))
@

The result is built directly: every outcome goes through @f@, and the
probabilities of the outcomes that land on the same value add up. That takes
time proportional to the number of values of @a@ plus that of @b@. The product
takes time proportional to the two numbers multiplied.
-}
fmapD ::
  forall a b.
  (IX.MatIndex a, IX.MatIndex b) =>
  (a -> b) ->
  Dist a ->
  Dist b
fmapD f d = collect [(f x, p) | (x, p) <- toValues d]

-- Adds up the weights given for each outcome, in list order, and gives every
-- other outcome 0. The result is a distribution when the weights sum to 1.
collect :: forall a. (IX.MatIndex a) => [(a, Prob)] -> Dist a
collect xs = D (IX.matrixBuilder' (\(r, _) -> weights ! r))
  where
    weights = accumArray (+) 0 (0, IX.cardinality @a - 1) [(IX.toOrd x, p) | (x, p) <- xs]

-- | Applicative/Monoidal instance @unit@ function
unitD :: Dist ()
unitD = D (IX.one 1)

-- | Applicative/Monoidal instance @mult@ function
multD ::
  Dist a ->
  Dist b ->
  Dist (a, b)
multD (D a) (D b) = D (IX.kr a b)

{- | Selective instance function. The matrix must be column-stochastic
(non-negative entries, every column summing to 1) for the result to be a
distribution.
-}
selectD ::
  Dist (Either a b) ->
  IX.Matrix Prob a b ->
  Dist b
selectD (D d) m = D (IX.select d m)

-- | Monad instance 'return' function
returnD ::
  (IX.MatIndex a) =>
  a ->
  Dist a
returnD = D . IX.point

{- | Monad instance '(>>=)' function. The continuation is a matrix whose column
@x@ is the distribution to continue with from @x@, so it has to be
column-stochastic (no negative entries, and @bang . m == bang@) for the result
to be a distribution.
-}
bindD ::
  Dist a ->
  IX.Matrix Prob a b ->
  Dist b
bindD (D d) m = D (m `IX.comp` d)

-- | Extract probabilities given an Event.
(??) ::
  (IX.MatIndex a) =>
  (a -> Bool) ->
  Dist a ->
  Prob
(??) p d = sum [q | (x, q) <- toValues d, p x]

-- Distribution construction

{- | Constructs a Bernoulli distribution over a two-valued type. The given
probability goes to the value numbered 0 (@False@ for 'Bool'), the rest to the
value numbered 1. Throws a runtime error if the probability is outside
@[0, 1]@.
-}
choose :: (HasCallStack, IX.DimOf a ~ (I.U I.:+: I.U)) => Prob -> Dist a
choose prob
  | prob >= 0 && prob <= 1 = D (IX.M (I.Fork (I.One prob) (I.One (1 - prob))))
  | otherwise = error ("LAoP.Dist.choose: probability " ++ show prob ++ " is outside [0, 1]")

{- | Creates a distribution over the given outcomes, weighting them by a shape
function sampled at evenly spaced points of @[0, 1]@. A single outcome gets
all the mass. Throws a runtime error when the shape function is negative at
one of the points, or zero at all of them (see 'fromFreqs').
-}
shape :: (HasCallStack, IX.MatIndex a) => (Prob -> Prob) -> NonEmpty a -> Dist a
shape _ (x :| []) = returnD x
shape f xs =
  let incr = 1 / fromIntegral (length xs - 1)
      ps = map f (iterate (+ incr) 0)
   in fromFreqs (zip (NE.toList xs) ps)

-- | Constructs a Linear distribution
linear :: (IX.MatIndex a) => NonEmpty a -> Dist a
linear = shape id

-- | Constructs an Uniform distribution
uniform :: (IX.MatIndex a) => NonEmpty a -> Dist a
uniform = shape (const 1)

-- | Constructs a Negative Exponential distribution
negExp :: (IX.MatIndex a) => NonEmpty a -> Dist a
negExp = shape (\x -> exp (-x))

-- | Constructs a Normal distribution
normal :: (IX.MatIndex a) => NonEmpty a -> Dist a
normal = shape (normalCurve 0.5 0.5)

{- | Builds a distribution from non-negative weights, normalising them so they
sum to 1. Weights given for the same outcome more than once are added up, and
outcomes that are not listed get probability 0. Throws a runtime error if a
weight is negative (or @NaN@), or if the weights do not have a positive sum.
-}
fromFreqs :: forall a. (HasCallStack, IX.MatIndex a) => [(a, Prob)] -> Dist a
fromFreqs xs
  | (w : _) <- [p | (_, p) <- xs, not (p >= 0)] =
      error ("LAoP.Dist.fromFreqs: weight " ++ show w ++ " is not >= 0")
  | total <= 0 = error "LAoP.Dist.fromFreqs: the weights must have a positive sum"
  | otherwise = D (IX.emap (/ total) weights)
  where
    D weights = collect xs
    total = sum [p | (_, p) <- xs]

-- | Transforms a 'Dist' into a list of pairs.
toValues :: forall a. (IX.MatIndex a) => Dist a -> [(a, Prob)]
toValues (D d) = zip (map IX.fromOrd [0 .. IX.cardinality @a - 1]) (IX.toList d)

-- | Pretty print a distribution, most likely outcome first.
prettyDist :: forall a. (Show a, IX.MatIndex a) => Dist a -> String
prettyDist d =
  let values = sortBy (\(_, pp1) (_, pp2) -> compare pp2 pp1) (toValues @a d)
      w = maximum (map (length . show . fst) values)
   in concatMap
        (\(x, p) -> showR w x ++ ' ' : showProb p ++ "\n")
        values
  where
    showProb p = show (p * 100) ++ "%"
    showR w x = let s = show x in s ++ replicate (w - length s) ' '

-- | Pretty print a distribution to @stdout@
prettyPrintDist :: forall a. (Show a, IX.MatIndex a) => Dist a -> IO ()
prettyPrintDist = putStrLn . prettyDist @a

-- Auxiliary

normalCurve :: Prob -> Prob -> Prob -> Prob
normalCurve mean dev x =
  let u = (x - mean) / dev
   in exp (-(u ^ (2 :: Int)) / 2) / sqrt (2 * pi)
