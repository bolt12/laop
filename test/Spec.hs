module Main (main) where

import Examples.Readme qualified as RD

main :: IO ()
main = do
  putStrLn ("Testing if README works" :: String)
  RD.exec
