module Main (main) where

import THandler
import TParser
import TUtils

main :: IO ()
main = do
  testUtils
  testHandler
  testParser