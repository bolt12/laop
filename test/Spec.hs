module Main (main) where

import qualified Examples.Readme as RD

main :: IO ()
main = do
  putStrLn ("Testing if README works" :: String)
  RD.exec
