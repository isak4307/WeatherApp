{-# LANGUAGE OverloadedStrings #-}

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
  T.pack (show w)
    <> T.pack "\n"
    <> "--------------------------------- \n"
    <> displayWeather weatherData (fromJust (date w))

-- | Display the weather data
displayWeather :: WeatherData -> UTCTime -> Text
displayWeather weatherData targetTime =
  case getSpecificTimeSeries weatherData targetTime of
    Nothing -> T.pack $ show $ MissingVal "No weather data available for the given time."
    Just timeData' ->
      case timeData' ^. instant of
        Nothing -> T.pack $ show $ MissingVal "No instant data available."
        Just instant' -> case instant' ^. detail of
          Nothing -> T.pack $ show $ MissingVal "No data available."
          Just details' ->
            let weather = fromMaybe (T.pack $ show $ MissingVal "No weather data available") $ do
                  sumDetail <- timeData' ^. next1Hour
                  summ <- sumDetail ^. summary
                  summ ^. symbolCode
             in T.pack $
                  "- WeatherCondition: "
                    <> show weather
                    <> "\n"
                    <> show details'
                    <> "\n Weather data provided by MET Norway"
                    <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- | Display the help menu
displayCommands :: Text
displayCommands =
  "Here are the available commands:\n\n"
    <> " current: Display the current weather given a city.\n"
    <> " !current City,?Country\n\n"
    <> " weather: Display the weather given a city, country and date.\n"
    <> " The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " !weather City,?Country, Date\n\n"
    <> " location: Display the latitude and longitude values given a city (and optionally a country) rounded to 4 decimals.\n"
    <> " !location City,?Country\n\n"
    <> " minMax: Get the highest and lowest temperature given city,country and date\n"
    <> " The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " !minMax City,Country,date\n\n"
    <> " !quit: Exit the application\n\n"
    <> " !help: Display all available commands"