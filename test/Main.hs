-- \|
-- Module      : Main
-- Description: Gather all testfiles and execute all of the tests by running cabal test
module Main (main) where

import THandler
import TParser
import TUtils

main :: IO ()
main = do
  testUtils
  testHandler
  testParser