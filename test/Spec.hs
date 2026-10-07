module Main (main) where

import qualified Examples.Quantum         as QU
import qualified Examples.Readme          as RD
import qualified Examples.Spreadsheets    as SS
import           Test.Dist.Properties
import           Test.Index.Properties
import           Test.Matrix.Composition
import           Test.Matrix.Properties
import           Test.Matrix.Reference
import           Test.Nat.Properties
import           Test.Relation.Properties
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "LAoP"
      [ matrixPropertyTests
      , matrixReferenceTests
      , relationPropertyTests
      , distPropertyTests
      , indexPropertyTests
      , natPropertyTests
      , compositionTests
      , testGroup "Examples"
          [ testGroup "README" (map check RD.checks)
          , testGroup "Quantum" (map check QU.checks)
          , testGroup "Spreadsheets" (map check SS.checks)
          ]
      ]
  where
    check (name, ok) = testCase name (assertBool name ok)
