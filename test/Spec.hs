module Main (main) where

import qualified Examples.Quantum      as QU
import qualified Examples.Readme       as RD
import qualified Examples.Spreadsheets as SS
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "LAoP"
      [ testGroup "Examples"
          [ testGroup "README" (map check RD.checks)
          , testGroup "Quantum" (map check QU.checks)
          , testGroup "Spreadsheets" (map check SS.checks)
          ]
      ]
  where
    check (name, ok) = testCase name (assertBool name ok)
