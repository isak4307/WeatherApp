{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Display
-- Description : Functions that displays all the output values.
module Display where

import Control.Lens ((^.))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Types
import Utils

-- | Construct a displayer for the weather data
display :: WeatherData -> Weather -> Text
display weatherData w =
  toText w
    <> "\n"
    <> displayWeather weatherData (fromJust (date w))

-- | Get the next 7 days of timeseries provided the date
displayWeekly :: [(Maybe TimeSeriesData, UTCTime)] -> Text
displayWeekly [] = ""
displayWeekly (v : vs) =
  let tsd = fst v
      time' = snd v
   in case tsd of
        Nothing -> toText $ MissingVal "No weather data available for the given time."
        Just timeData' -> fromMaybe (toText $ MissingVal "No data available.") $ do
          instant' <- timeData' ^. instant
          details' <- instant' ^. detail
          let weather = fromMaybe (toText $ MissingVal "No weather summary available ") $ do
                sumDet <- timeData' ^. next1Hour
                summ <- sumDet ^. summary
                summ ^. symbolCode
          let temp = case details' ^. airTemperature of
                Nothing -> toText $ MissingVal "N/A"
                Just tmp -> T.pack $ show tmp
          return $
            ( "-Date: "
                <> toText time'
                <> "\t"
                <> weather
                <> "\t Temperature: "
                <> temp
                <> "C"
            )
              <> "\n"
              <> displayWeekly vs

-- | Display the weather data given a date
displayWeather :: WeatherData -> UTCTime -> Text
displayWeather weatherData targetTime =
  case getSpecificTimeSeries weatherData (roundToNearestHour targetTime) of
    Nothing -> toText $ MissingVal "Unable to fetch the weather data for the given time."
    Just timeData' -> fromMaybe (toText $ MissingVal "N/A") $ do
      instant' <- timeData' ^. instant
      details' <- instant' ^. detail
      let weather = fromMaybe (toText $ MissingVal "No weather data available") $ do
            sumDetail <- timeData' ^. next1Hour
            summ <- sumDetail ^. summary
            summ ^. symbolCode
      return $
        "- WeatherCondition: "
          <> weather
          <> "\n"
          <> toText details'
          <> "\n Weather data provided by MET Norway"
          <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- | Display the sunrise and sunset data
displaySun :: SunData -> Text
displaySun sd =
  case sd ^. sunProperties of
    Nothing -> toText $ MissingVal "Unable to fetch the sunrise and sunset data."
    Just sunProps ->
      do
        let sunriseTxt =
              case sunProps ^. sunrise of
                Nothing -> toText $ MissingVal "Cannot find data for sunrise."
                Just sunrise' -> toText sunrise'
        let sunsetTxt = case sunProps ^. sunset of
              Nothing -> toText $ MissingVal "Cannot find data for sunset."
              Just sunset' -> toText sunset'
        sunriseTxt <> "\n\n" <> sunsetTxt <> "\n\n"
        <> "Sunset and Sunrise data provided by MET Norway"
        <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- | Display the help menu
displayCommands :: Text
displayCommands =
  "Here are the available commands:\n\n"
    <> " weather: Display the weather given a city, optionally a country and optionally a date.\n"
    <> " The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " By default, the date would be the current date and time \n"
    <> " !weather City,?Country,?Date\n\n"
    <> " location: Display the latitude and longitude values given a city (and optionally a country) rounded to 4 decimals.\n"
    <> " !location City,?Country\n\n"
    <> " minMax: Get the highest and lowest temperature given city,optionally a country and optionally a date \n"
    <> " The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " !minMax City,?Country,?Date\n\n"
    <> " week: Display the weather data for the next 7 days given a city, optionally a country and optionally a date\n"
    <> " The weather data is rounded to the nearest quarter of the day, if it doesn't have the weather data for the exact time.\n"
    <> " !week City, ?Country,?Date\n\n"
    <> " sun: Display the time and direction of sunrise and sunset given the city, optionally a country and date\n"
    <> " !sun City, ?Country, Date\n\n"
    <> " !quit: Exit the application\n\n"
    <> " !help: Display all available commands"