{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TParser where

import Data.Char (isDigit, isPrint)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Parser (parseGeoLocation, parseWeather)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    forAll,
    listOf1,
    quickCheck,
    suchThat,
  )
import Text.Megaparsec (runParser)
import Text.Printf (printf)
import Types
  ( Location (cityL, countryL),
    Weather (city, country, date),
  )

testParser :: IO ()
testParser = do
  -- Check that weather is able to parse without date
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyTextWithoutDigits $ \country' ->
      test_parseCurrentWeather city' country'
  -- Check that weather is able to parse every input field
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyTextWithoutDigits $ \country' ->
      forAll dateGen $ \date' ->
        test_parseWeather city' country' date'
  -- Check that weather is able to parse city and date
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll dateGen $ \date' ->
      test_parseWeatherDate city' date'
  -- Check that Location parsing works
  quickCheck $ forAll nonEmptyText $ \city' -> test_parseGeoLocation city'



test_parseWeatherDate :: Text -> Text -> Bool
test_parseWeatherDate c d = test_parseWeather c "" d

test_parseCurrentWeather :: Text -> Text -> Bool
test_parseCurrentWeather city' country' =
  let inp = if T.null country' then city' else city' <> "," <> country'
      res = runParser parseWeather "" inp
   in case res of
        Left _ -> False
        Right weather ->
          let compareCity = city' == city weather
              compareCountry = case country weather of
                Just v -> country' == v
                Nothing -> T.null country'
           in (compareCity == compareCountry) == isNothing (date weather)

test_parseWeather :: Text -> Text -> Text -> Bool
test_parseWeather city' country' date' =
  let inp = city' <> "," <> country' <> "," <> date'
      res = runParser parseWeather "" inp
   in case res of
        Left _ -> False
        Right weather ->
          let compareCity = city' == city weather
              compareCountry = case country weather of
                Just v -> country' == v
                Nothing -> T.null country'
              compareDate = case date weather of
                Just d' -> date' == T.pack (formatTime defaultTimeLocale "%FT%T%QZ" d')
                Nothing -> T.null date'
           in compareCity && compareCountry && compareDate

test_parseGeoLocation :: Text -> Text -> Bool
test_parseGeoLocation city' country' =
  let inp = if T.null country' then city' else city' <> "," <> country'
      res = runParser parseGeoLocation "" inp
   in case res of
        Left _ -> False
        Right loc ->
          let compareCity = city' == cityL loc
              compareCountry = case countryL loc of
                Just v -> country' == v
                Nothing -> T.null country'
           in compareCity == compareCountry

-- | Generate nonempty text + no digits allowed since parser can't allow country to have digits
nonEmptyTextWithoutDigits :: Gen Text
nonEmptyTextWithoutDigits = do
  T.filter (not . isDigit) <$> nonEmptyText

-- | Generate nonempty text for city since that is mandatory to not be empty
nonEmptyText :: Gen Text
nonEmptyText = T.pack <$> listOf1 (arbitrary `suchThat` validChar)
               where
                validChar c = c/=' ' && validChars c && c/='\\'

validChars :: Char -> Bool
validChars c= c/=',' && isPrint c

-- | Generate Mock date/utctimes in Text
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


instance Arbitrary Text where
  arbitrary = T.pack . filter validChars <$> arbitrary