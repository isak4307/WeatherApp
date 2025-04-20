{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenUtils where

import Data.Char (isAscii, isDigit, isPrint)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Gen, arbitrary, choose, listOf1, suchThat)
import Text.Printf (printf)

-- | Generate mock utctimes used for tests
dateGen :: Gen Text
dateGen = do
  year :: Int <- choose (1000, 9999)
  month :: Int <- choose (1, 12)
  day :: Int <- choose (1, 26) -- can't bother checking when it should be 31 or 30 depending on the month
  hour :: Int <- choose (0, 23)
  minute :: Int <- choose (0, 59)
  second :: Int <- choose (0, 59)
  let utcT = T.pack (printf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute second)
  return utcT

-- | Generate nonempty text + no digits allowed since parser can't allow country to have digits
nonEmptyTextWithoutDigits :: Gen Text
nonEmptyTextWithoutDigits = do
  T.filter (not . isDigit) <$> nonEmptyText

-- | Generate nonempty text since that is mandatory for certain inputs to not be empty
nonEmptyText :: Gen Text
nonEmptyText = T.pack <$> listOf1 (arbitrary `suchThat` validChar)
  where
    validChar c = c /= ' ' && validChars c

validChars :: Char -> Bool
validChars c = c /= ',' && isPrint c && c /= '\\' && isAscii c
