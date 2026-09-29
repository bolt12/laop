-- Quantum gates as complex matrices, with a check that CNOT after a Hadamard
-- on the first qubit takes |00> to the Bell state (|00> + |11>) / sqrt 2.
module Examples.Quantum (
  cnot,
  ccnot,
  had,
  bell,
  checks,
) where

import           Data.Complex
import           LAoP.Matrix.Indexed
import           LAoP.Utils
import           Prelude             hiding (id, (.))

xor :: (Bool, Bool) -> Bool
xor (False, b) = b
xor (True, b)  = not b

-- | Controlled NOT: flips the second qubit when the first is set.
cnot :: Matrix (Complex Double) (Bool, Bool) (Bool, Bool)
cnot = kr fstM (fromF xor)

-- | Toffoli gate: flips the third qubit when the first two are set.
ccnot :: (Num e) => Matrix e ((Bool, Bool), Bool) ((Bool, Bool), Bool)
ccnot = kr fstM (fromF f)
  where
    f = xor . both (uncurry (&&)) id
    both g h (a, b) = (g a, h b)

-- | Hadamard gate.
had :: Matrix (Complex Double) Bool Bool
had = (1 / sqrt 2) .| fromLists [[1, 1], [1, -1]]

-- | Circuit preparing a Bell pair from |00>.
bell :: Matrix (Complex Double) (Bool, Bool) (Bool, Bool)
bell = cnot . (had >< iden)

checks :: [(String, Bool)]
checks =
  [ ("bell |00> == (|00> + |11>) / sqrt 2", and (zipWith close amplitudes expected))
  , ("Toffoli flips the target only when both controls are set", toffoliOk)
  ]
  where
    amplitudes = toList (bell . point (False, False))
    h = 1 / sqrt 2
    expected = [h, 0, 0, h]
    close x y = magnitude (x - y) < 1e-12
    toffoliOk =
      and
        [ toList (ccnot . point @Int ((a, b), c)) == toList (point @Int ((a, b), c /= (a && b)))
        | a <- [False, True]
        , b <- [False, True]
        , c <- [False, True]
        ]
