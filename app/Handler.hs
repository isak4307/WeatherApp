{-# LANGUAGE OverloadedStrings #-}

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
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr (InvalidFormat (T.pack $ show err))
    Right loc -> fetchGeoLocation loc

-- | Handle parsing and getting the WeatherData
handleWeather :: Text -> ExceptT ErrorTypes IO (WeatherData, Weather)
handleWeather args =
  case runParser parseWeather "" args of
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr (InvalidFormat (T.pack $ show err))
    Right weather -> do
      wData <- fetchWeather weather
      return (wData, weather)

-- | Handle parsing and getting the Min Max temperature
handleMinMax :: WeatherData -> UTCTime -> Weather -> ExceptT ErrorTypes IO Text
handleMinMax wData d weather = do
  let ts = filterWeatherByDate wData (utctDay d)
  minTempRes <- case getMin ts d of
    Just (ts', minTemp) ->
      case ts' ^. time of
        Just minTempDate -> return $ "Min temperature: " <> show minTemp <> " with this weather:\n" <> T.unpack (display wData weather {date = Just minTempDate}) <> "\n"
        Nothing -> throwError $ MissingVal "Unable to get corresponding date for the lowest temperature\n"
    Nothing -> throwError $ MissingVal "Unable to fetch the lowest temperature\n"
  maxTempRes <- case getMax ts d of
    Just (ts', maxTemp) ->
      case ts' ^. time of
        Just maxTempDate -> return $ "Max temperature: " <> show maxTemp <> " with this weather:\n" <> T.unpack (display wData weather {date = Just maxTempDate})
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
      -- Get only the first  location (not sure if that is a good idea)
      let output = T.pack $ "Fetched location:\n " <> show x <> "\n Data provided by Nominatim/OpenStreetMap (https://nominatim.org/)"
       in return output
    Right [] -> return $ T.pack $ show $ MissingVal "There are no values with the given input"

-- \| Handle Getting weather data given time
handleCommand (WeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) -> do
      time' <- liftIO $
        case date weather of
          Just d -> pure d
          Nothing -> currentTime

      return $ display wData weather {date = Just time'}

-- \| Handle Getting the highest and lowest temperature for the given day
handleCommand (MinMaxWeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) ->
      case date weather of
        Just d -> do
          res <- liftIO $ runExceptT $ handleMinMax wData d weather
          case res of
            Left e -> throwError e
            Right minMax -> return minMax
        Nothing -> return $ T.pack $ show $ MissingVal "Unable to fetch the appropriate date"

-- \| Handle weekly weather data
handleCommand (WeekWeatherCmd args) = do
  result <- liftIO $ runExceptT $ handleWeather args
  case result of
    Left err -> throwError err
    Right (wData, weather) -> do
      time' <- liftIO $
        case date weather of
          Just d -> pure d
          Nothing -> currentTime
      return $
        T.pack (show $ city weather)
          <> "s weather for the next 7 days:\n"
          <> displayWeekly (getWeeklyTimeSeries wData time')
          <> " Weather data provided by MET Norway"
          <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- \| Handle sunset/sunrise command
handleCommand (SunCmd args) = do
  result <- liftIO $ runExceptT $ handleSun args
  case result of
    Left err -> throwError err
    Right v ->
      return $
              (T.pack . show $ fst v)
              <> "\n"
              <> displaySun (snd v)
              <> "Sunset and Sunrise data provided by MET Norway"
              <> " and Nominatim/OpenStreetMap (Latitude and Longitude)"

-- \| Handle mistyped/non-existing commands
handleCommand _ = return $ T.pack $ show $ UnknownCommand "Something went wrong with the given command"