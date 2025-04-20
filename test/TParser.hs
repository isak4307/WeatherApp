{-# LANGUAGE OverloadedStrings #-}

module TParser where

import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GenUtils
import Parser
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    forAll,
    quickCheck,
  )
import Text.Megaparsec (runParser)
import Types

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
  -- Check that weather is able to parse with city and date
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll dateGen $ \date' ->
      test_parseWeather city' "" date'
  -- Check that weather is able to parse without country
  quickCheck $ forAll nonEmptyText $ \city' -> test_parseCurrentWeather city' ""

  -- Check that Location parsing
  quickCheck $ forAll nonEmptyText $ \city' -> test_parseGeoLocation city' ""
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyText $ \country' -> test_parseGeoLocation city' country'

test_parseCurrentWeather :: Text -> Text -> Bool
test_parseCurrentWeather city' country' =
  let inp = if T.null (T.strip country') then city' else city' <> "," <> country'
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

instance Arbitrary Text where
  arbitrary = T.pack . filter validChars <$> arbitrary