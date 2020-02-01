-----------------------------------------------------------------------------
-- |
-- Module     : LAoP.Utils
-- Copyright  : (c) Armando Santos 2019-2020
-- Maintainer : armandoifsantos@gmail.com
-- Stability  : experimental
--
-- __LAoP__ is a library for algebraic (inductive) construction and manipulation of matrices
-- in Haskell. See <https://github.com/bolt12/master-thesis my Msc Thesis> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides the 'Natural' data type.
-- The semantic associated with this data type is that
-- it's meant to be a restricted 'Int' value.
--
-----------------------------------------------------------------------------

module LAoP.Utils
  ( -- | Utility module that provides the 'Natural' data type.
    -- The semantic associated with this data type is that
    -- it's meant to be a restricted 'Int' value. For example
    -- the type @Natural 1 6@ can only be instanciated with @nat n@
    -- where @1 <= n <= 6@. Why, You might ask, because with normal
    -- 'Int's it is not possible to have a decent @Enum (Int, Int)@
    -- instance. See the following probabilistic programming model as and
    -- example:
    --
    -- We want to calculate the probability of the sum of two dice throws.
    -- To do this we start by defining the sample space: 
    --
    -- @
    -- type SampleSpace = Int -- We think 'Int' are enough
    --
    -- die :: Dist Int 6
    -- die = unifrom [1..6] 
    --
    -- -- Promote 'Int' addition to a matrix
    -- addM = fromF (uncurry (+)) -- Impossible
    -- @
    --
    -- The last line is impossible because @(Int, Int)@ does not have
    -- a good 'Enum' instance: @[(0, 1), (0, 2), .. (0, maxBound), (1, 0),
    -- ..]@. And we'd like the addition matrix to be of 36 columns by 12
    -- rows but limited to integers up to @6@!
    --
    -- One way to solve this issue is by defining and auxilary data type to
    -- represent the sample space:
    --
    -- @
    -- data SampleSpace = S1 | S2 | S3 | S4 | S5 | S6
    --   deriving (Show, Eq, Enum, Bounded) -- Enum and Bounded are
    --   important
    -- @
    --
    -- And write the sample space addition function:
    --
    -- @
    -- ssAdd :: SampleSpace -> SampleSpace -> Int
    -- ssAdd a b = (fromEnum a + 1) + (fromEnum b + 1)
    -- @
    --
    -- And then promote that function to matrix and everything is alright:
    --
    -- @
    -- ssAddM = fromF' (uncurry ssAdd)
    --
    -- dieSumProb = ssAddM `comp` (khatri die die)
    -- @
    --
    -- This is a nice solution for small sample spaces. But for larger ones 
    -- it is not feasible to write a data type with hundreds of constructors
    -- and then write manipulation functions that need to deal with them.
    -- To mitigate this limitation the 'Natural' type comes a long way and
    -- allows one to model the sample in an easier way. See for instance:
    --
    -- @
    -- ssAdd :: Natural 1 6 -> Natural 1 6 -> Natural 1 12
    -- ssAdd = coerceNat (+) 
    -- 
    -- ssAddM = fromF' (uncurry sumSS)
    -- 
    -- die :: Dist (Natural 1 6) 6
    -- die = uniform [nat @1 @6 1 .. nat 6]
    --
    -- dieSumProb = ssAddM `comp` (khatri die die)
    -- @
    --
    -- * 'Natural' data type
    Natural,
    nat,

    -- * Coerce auxiliar functions to help promote 'Int' typed functions to
    -- 'Natural' typed functions.
    coerceNat,
    coerceNat2,
    coerceNat3,

    -- * Powerset data type
    List (..)
  )
where

import LAoP.Utils.Internal
