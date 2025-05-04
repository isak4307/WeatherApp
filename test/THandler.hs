{-# LANGUAGE OverloadedStrings #-}

-- \|
-- Module      : THandler
-- Description: Contains all QuickChecks that are related to the functions found in Handler.hs

module THandler where

import qualified Data.Text as T
import GenUtils
import Parser (parseCommand)
import Test.QuickCheck
  ( Property,
    elements,
    forAll,
    quickCheck,
  )
import Types (PError (EmptyInput, UnknownCommand))

testHandler :: IO ()
testHandler = do
  -- Check that parsing a command without any arguments gives the error EmptyInput
  quickCheck $ parseCommand "" == Left EmptyInput
  -- Check that inputting valid commands will be successfully parsed
  quickCheck test_valid_parseCommand
  -- Check that invalid commands won't be successfully parsed
  quickCheck test_invalid_parseCommand

-- | parseCommand should only parse certain commands that start with ! and whether they have arguments or not
test_valid_parseCommand :: Property
test_valid_parseCommand = forAll (elements ["!quit", "!help", "!weather", "!location", "!minMax", "!week", "!sun"]) $ \cmd ->
  case parseCommand (T.pack cmd) of
    -- these commands go to Left ArgumentError since these need to have arguments
    Left _ -> T.pack cmd `elem` ["!minMax", "!weather", "!location", "!week", "!sun"]
    -- these commands go to Right Text, since these don't need to have arguments
    Right _ -> T.pack cmd `elem` ["!quit", "!help"]

-- | Check that input that contains ! at the start and not a valid command is not parsed
test_invalid_parseCommand :: GenText -> Bool
test_invalid_parseCommand (GenText input) =
  let cmdInp = "!" <> input
   in case parseCommand cmdInp of
        Left UnknownCommand {} -> True
        _ -> False
