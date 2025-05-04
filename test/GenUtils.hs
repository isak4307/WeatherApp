{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenUtils where

import Data.Char (isAscii, isDigit, isPrint)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, listOf1, suchThat)
import Test.QuickCheck.Instances.Time ()

-- | Generate mock utctimes in text format used for tests
dateGen :: Gen Text
dateGen = do
  utcTime <- arbitrary :: Gen UTCTime
  let formatT = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utcTime
  return $ T.pack formatT

dateOnlyGen :: Gen Text
dateOnlyGen = do
  utcTime <- arbitrary :: Gen UTCTime
  let formatT = formatTime defaultTimeLocale "%Y-%m-%d" utcTime
  return $ T.pack formatT

-- | Generate nonempty text + no digits allowed since parser can't allow country to have digits
nonEmptyTextWithoutDigits :: Gen Text
nonEmptyTextWithoutDigits = do
  T.filter (not . isDigit) <$> nonEmptyText

-- | Generate nonempty text since that is mandatory for city inputs to not be empty
nonEmptyText :: Gen Text
nonEmptyText = T.pack <$> listOf1 (arbitrary `suchThat` validChar)
  where
    validChar c = c /= ' ' && validChars c

-- | Filters out all invalid characters such as unicode (with \) ',' and non-printable characters (since the results needs to be printable)
validChars :: Char -> Bool
validChars c = c /= ',' && isPrint c && c /= '\\' && isAscii c

-- | Created a wrapper type so that there won't be any orphan warnings
newtype GenText = GenText Text
  deriving (Show)

instance Arbitrary GenText where
  arbitrary = GenText . T.pack . filter validChars <$> arbitrary