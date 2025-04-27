{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenUtils where

import Data.Char (isAscii, isDigit, isPrint)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, listOf1, suchThat)
import Text.Printf (printf)

-- | Generate mock utctimes used for tests
dateGen :: Gen Text
dateGen = do
  year :: Int <- choose (1000, 9999)
  month :: Int <- choose (1, 12)
  day :: Int <- choose (1, 27) -- can't bother checking when it should be 31 or 30 depending on the month
  hour :: Int <- choose (0, 23)
  minute :: Int <- choose (0, 59)
  second :: Int <- choose (0, 59)
  let utcT = T.pack (printf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute second)
  return utcT

-- | Generate mock dates without the time used for parseSun tests
dateOnlyGen :: Gen Text
dateOnlyGen = do
  year :: Int <- choose (1000, 9999)
  month :: Int <- choose (1, 12)
  day :: Int <- choose (1, 28)
  return (T.pack (printf "%04d-%02d-%02d" year month day))

genUTCTime :: Gen UTCTime
genUTCTime = do
  date <- dateGen
  let parsedTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack date) :: Maybe UTCTime
  case parsedTime of
    Just utcTime -> return utcTime
    Nothing -> error "Generated text could not be parsed into UTCTime"

-- | Generate nonempty text + no digits allowed since parser can't allow country to have digits
nonEmptyTextWithoutDigits :: Gen Text
nonEmptyTextWithoutDigits = do
  T.filter (not . isDigit) <$> nonEmptyText

-- | Generate nonempty text since that is mandatory for certain inputs to not be empty
nonEmptyText :: Gen Text
nonEmptyText = T.pack <$> listOf1 (arbitrary `suchThat` validChar)
  where
    validChar c = c /= ' ' && validChars c

-- | filter that filters out all invalid characters such as unicode (with \) ',' and non-printable characters (since the results needs to be printable)
validChars :: Char -> Bool
validChars c = c /= ',' && isPrint c && c /= '\\' && isAscii c

-- | Created a wrapper type so that there won't be any orphans
newtype GenText = GenText Text
  deriving (Show)

instance Arbitrary GenText where
  arbitrary = GenText . T.pack . filter validChars <$> arbitrary