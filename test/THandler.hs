{-# LANGUAGE OverloadedStrings #-}

module THandler where

import Data.Text (Text)
import qualified Data.Text as T
import Parser (parseCommand)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    elements,
    forAll,
    quickCheck,
  )
import Types (PError (EmptyInput, UnknownCommand))

testHandler :: IO ()
testHandler = do
  -- Check that parsing a command without any arguments gives the error EmptyInput
  quickCheck $ parseCommand "" == Left EmptyInput
  -- Check that giving valid commands will be successfully parsed
  quickCheck test_valid_parseCommand
  -- Checkt that invalid commands won't be successfully parsed
  quickCheck test_invalid_parseCommand

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

-- | parseCommand should only parse certain commands that start with ! and whether they have arguments or not
test_valid_parseCommand :: Property
test_valid_parseCommand = forAll (elements ["!quit", "!help", "!weather", "!location", "!minMax"]) $ \cmd ->
  case parseCommand (T.pack cmd) of
    -- these commands go to Left ArgumentError since these need to have arguments
    Left _ -> T.pack cmd `elem` ["!minMax", "!weather", "!location"]
    -- these commands go to Right Text, since these don't need to have arguments
    Right _ -> T.pack cmd `elem` ["!quit", "!help"]

test_invalid_parseCommand :: Text -> Bool
test_invalid_parseCommand input =
  let cmdInp = "!" <> input
   in case parseCommand cmdInp of
        Left UnknownCommand {} -> True
        _ -> False