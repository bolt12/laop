{-# LANGUAGE StandaloneDeriving #-}
module Quantum where

import LAoP.Utils
import LAoP.Matrix.Type
import Data.Complex
import Prelude hiding (id, (.))

deriving instance Ord a => Ord (Complex a)

xor :: (Bool, Bool) -> Bool
xor (False, b) = b
xor (True, b) = not b

ker :: Num e => Matrix e a b -> Matrix e a a
ker m = tr m . m

cnot :: Matrix (Complex Double) (Bool, Bool) (Bool, Bool)
cnot = kr fstM (fromF xor)

ccnot :: (Num e, Ord e) => Matrix e ((Bool, Bool), Bool) ((Bool, Bool), Bool)
ccnot = kr fstM (fromF f)
  where
    f = xor . (tp (uncurry (&&)) id)
    tp f g (a,b) = (f a, g b)

had :: Matrix (Complex Double) Bool Bool
had = (1/sqrt 2) .| fromLists [[1, 1], [1, -1]]

bell :: Matrix (Complex Double) (Bool, Bool) (Bool, Bool)
bell = cnot . (had >< iden)
