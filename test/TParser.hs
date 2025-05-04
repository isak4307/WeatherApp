{-# LANGUAGE OverloadedStrings #-}

module TParser where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GenUtils
import Parser
import Test.QuickCheck
  ( forAll,
    quickCheck,
  )
import Text.Megaparsec (runParser)
import Types

testParser :: IO ()
testParser = do
  -- Check that weather is able to parse with city and country
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyTextWithoutDigits $ \country' ->
      test_parseCurrentWeather (GenText city') (GenText country')
  -- Check that weather is able to parse with every input field
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyTextWithoutDigits $ \country' ->
      forAll dateGen $ \date' ->
        test_parseWeather (GenText city') (GenText country') (GenText date')
  -- Check that weather is able to parse with city and date
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll dateGen $ \date' ->
      test_parseWeather (GenText city') (GenText "") (GenText date')
  -- Check that weather is able to parse with city only
  quickCheck $ forAll nonEmptyText $ \city' -> test_parseCurrentWeather (GenText city') (GenText "")

  -- Check that Location parses with and without country
  quickCheck $ forAll nonEmptyText $ \city' -> test_parseGeoLocation (GenText city') (GenText "")
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyText $ \country' -> test_parseGeoLocation (GenText city') (GenText country')

  -- Check that Sun parses with and without country
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll nonEmptyTextWithoutDigits $ \country' ->
      forAll dateOnlyGen $ \date' ->
        test_parseSun (GenText city') (GenText country') (GenText date')
  quickCheck $ forAll nonEmptyText $ \city' ->
    forAll dateOnlyGen $ \date' ->
      test_parseSun (GenText city') (GenText "") (GenText date')

-- | Check that city and country inputted are the same after being parsed , and that date is Nothing
test_parseCurrentWeather :: GenText -> GenText -> Bool
test_parseCurrentWeather (GenText city') (GenText country') =
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

-- | Check that the inputted city country and date are still the same values after being parsed
test_parseWeather :: GenText -> GenText -> GenText -> Bool
test_parseWeather (GenText city') (GenText country') (GenText date') =
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

-- | Check that the inputted city and country have the same value after being parsed
test_parseGeoLocation :: GenText -> GenText -> Bool
test_parseGeoLocation (GenText city') (GenText country') =
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

-- | Check that the inputted city and country and date have the same value after being parsed
test_parseSun :: GenText -> GenText -> GenText -> Bool
test_parseSun (GenText city') (GenText country') (GenText date') =
  let inp = city' <> "," <> country' <> "," <> date'
      res = runParser parseSun "" inp
   in case res of
        Left _ -> False
        Right sun ->
          let compareCity = city' == cityS sun
              compareCountry = case countryS sun of
                Just v -> country' == v
                Nothing -> T.null country'
              compareDate = dateS sun == date'
           in compareCity && compareCountry && compareDate
