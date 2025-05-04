module Main (main) where

import THandler
import TParser
import TUtils
-- | Gather all testfiles and execute all of the tests by running cabal test
main :: IO ()
main = do
  testUtils
  testHandler
  testParser