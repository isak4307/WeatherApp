{-# LANGUAGE OverloadedStrings #-}

-- \|
-- Module      : Handler
-- Description : Contains all functions that works as a bridge amongst other
--               functions such as getting data from APIs and parsing input values
module Handler where

import API
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Display
import Parser
import Servant (throwError)
import Text.Megaparsec
  ( ParseErrorBundle (ParseErrorBundle),
    runParser,
  )
import Types
import Utils

-- | Handle parsing and getting the geoLocation
handleGeoLocation :: Text -> ExceptT ErrorTypes IO [GeoLocation]
handleGeoLocation args =
  case runParser parseGeoLocation "" args of
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr $ InvalidFormat $ T.pack $ show err
    Right loc -> fetchGeoLocation loc

-- | Handle parsing and getting the WeatherData
handleWeather :: Text -> ExceptT ErrorTypes IO (WeatherData, Weather)
handleWeather args =
  case runParser parseWeather "" args of
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr $ InvalidFormat $ T.pack $ show err
    Right weather -> do
      (wData, geoLoc) <- fetchWeather weather
      time' <- -- Input time should be rounded to nearest hour since weather data isn't minute precise
        case date weather of
          Just d -> pure d
          Nothing -> liftIO currentTime
      return (wData, weather {city = name geoLoc, date = Just $ roundToNearestHour time'})

-- | Handle getting the Min Max temperature
handleMinMax :: WeatherData -> UTCTime -> Weather -> ExceptT ErrorTypes IO Text
handleMinMax wData d weather = do
  let ts = filterWeatherByDate wData (utctDay d)
      roundedT = roundToNearestHour d
  minTempRes <- case getMin ts roundedT of
    Just (ts', minTemp) ->
      case ts' ^. time of
        Just minTempDate -> return $ "Min temperature: " <> show minTemp <> "C\n" <> T.unpack (display wData weather {date = Just minTempDate}) <> "\n"
        Nothing -> throwError $ MissingVal "Unable to get corresponding date for the lowest temperature\n"
    Nothing -> throwError $ MissingVal "Unable to fetch the lowest temperature\n"
  maxTempRes <- case getMax ts roundedT of
    Just (ts', maxTemp) ->
      case ts' ^. time of
        Just maxTempDate -> return $ "Max temperature: " <> show maxTemp <> "C\n" <> T.unpack (display wData weather {date = Just maxTempDate})
        Nothing -> throwError $ MissingVal "Unable to get the corresponding date for the highest temperature\n"
    Nothing -> throwError $ MissingVal "Unable to fetch the highest temperature\n"
  return (T.pack $ minTempRes <> maxTempRes)

-- | Handle parsing and getting the Sunrise and Sunset data
handleSun :: Text -> ExceptT ErrorTypes IO (Sun, SunData)
handleSun args =
  case runParser parseSun "" args of
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr $ InvalidFormat $ T.pack $ show err
    Right sun -> fetchSun sun

-- | Handler for all the commandtypes
handleCommand :: Cmd -> ExceptT ErrorTypes IO Text
-- \| Handle Location conversion to Lat Lon
handleCommand (LocationCmd args) = do
  result <- liftIO $ runExceptT $ handleGeoLocation args
  case result of
    Left err -> throwError err
    Right (x : _) ->
      -- Get only the first location
      let output = "Fetched location:\n " <> toText x <> "\n Data provided by Nominatim/OpenStreetMap (https://nominatim.org/)"
       in return output
    Right [] -> return $ toText $ MissingVal $ "There are no locations with the given input " <> args

-- \| Handle Getting weather data given time
handleCommand (WeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) -> return $ display wData weather

-- \| Handle Getting the highest and lowest temperature for the given day
handleCommand (MinMaxWeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) -> do
      Just date' <- pure $ date weather
      res <- liftIO $ runExceptT $ handleMinMax wData date' weather
      case res of
        Left e -> throwError e
        Right minMax -> return minMax

-- \| Handle weekly weather data
handleCommand (WeekWeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) -> do
      Just time' <- pure (date weather)
      return $
        toText weather
          <> "\n weather for the next 7 days:\n"
          <> displayWeekly (getWeeklyTimeSeries wData time')
          <> "\n Weather data provided by MET Norway"
          <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- \| Handle sunset/sunrise command
handleCommand (SunCmd args) = do
  result <- liftIO $ runExceptT $ handleSun args
  case result of
    Left err -> throwError err
    Right v ->
      return $
        toText (fst v)
          <> "\n"
          <> displaySun (snd v)

-- \| Handle mistyped/non-existing commands
handleCommand _ = return $ toText $ UnknownCommand "Something went wrong with the given command"