{-# LANGUAGE StandaloneDeriving #-}
module Quantum where

import LAoP.Matrix.Type
import Data.Complex

deriving instance Ord a => Ord (Complex a)

xor :: (Bool, Bool) -> Bool
xor (False, b) = b
xor (True, b) = not b

ker :: Num e => Matrix e a b -> Matrix e a a
ker m = tr m `comp` m

cnot :: Matrix (Complex Double) (Bool, Bool) (Bool, Bool)
cnot = khatri fstM (fromF xor)

ccnot :: Matrix (Complex Double) ((Bool, Bool), Bool) ((Bool, Bool), Bool)
ccnot = khatri fstM (fromF f)
  where
    f = xor . (tp (uncurry (&&)) id)
    tp f g (a,b) = (f a, g b)

had :: Matrix (Complex Double) Bool Bool
had = fromLists l
  where
    l = map (map (*(1/sqrt 2))) [[1, 1], [1, -1]]
