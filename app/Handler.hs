{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler where

import API
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Parser
import Servant (throwError)
import Text.Megaparsec
  ( ParseErrorBundle (ParseErrorBundle),
    runParser,
  )
import Types

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

-- | Handle parsing and getting the WeatherData
handleCurrentWeather :: Text -> ExceptT ErrorTypes IO (WeatherData, Weather)
handleCurrentWeather args =
  case runParser parseCurrentWeather "" args of
    Left (ParseErrorBundle _ err) -> throwError $ ParseErr (InvalidFormat (T.pack $ show err))
    Right weather -> do
      wData <- fetchWeather weather
      return (wData, weather)

-- | Handle Min Max weather
handleMinMax :: WeatherData -> UTCTime -> Weather -> ExceptT ErrorTypes IO Text
handleMinMax wData d weather = do
  let ts = filterWeatherByDate wData (utctDay d)
  minTempRes <- case getMin ts d of
    Just (ts', minTemp) ->
      case ts' ^. time of
        Just minTempDate -> return $ "Min temperature: " <> show minTemp <> " with this weather:\n" <> T.unpack (display wData weather {date = Just minTempDate})
        Nothing -> throwError $ MissingVal "Unable to get corresponding date for the lowest temperature\n"
    Nothing -> throwError $ MissingVal "Unable to fetch the lowest temperature\n"
  maxTempRes <- case getMax ts d of
    Just (ts', maxTemp) ->
      case ts' ^. time of
        Just maxTempDate -> return $ "Max temperature: " <> show maxTemp <> " with this weather:\n" <> T.unpack (display wData weather {date = Just maxTempDate})
        Nothing -> throwError $ MissingVal "Unable to get the corresponding date for the highest temperature\n"
    Nothing -> throwError $ MissingVal "Unable to fetch the highest temperature\n"
  return (T.pack $ minTempRes <> maxTempRes)

-- | Handler for all the commandtypes
handleCommand :: Cmd -> IO ()

-- | Handle Location conversion to Lat Lon
handleCommand (LocationCmd args) = do
  result <- runExceptT $ handleGeoLocation args
  case result of
    Left err -> print err
    Right (x : _) -> putStrLn $ "Fetched location:\n " <> show x -- Get only the first  location (not sure if that is a good idea)
    Right [] -> print $ MissingVal "There are no values with the given input"

-- \| Handle Getting weather data given time
handleCommand (WeatherCmd args) = do
  result <- runExceptT $ handleWeather args
  case result of
    Left err -> print err
    Right (wData, weather) -> do
      putStrLn $ T.unpack (display wData weather)

-- \| Handle Getting the current weather data
handleCommand (CurrentWeatherCmd args) = do
  result <- runExceptT $ handleCurrentWeather args
  case result of
    Left err -> print err
    Right (wData, weather) -> do
      time' <- liftIO $ maybe currentTime return (date weather)
      putStrLn $ T.unpack (display wData weather {date = Just time'})

-- \| Handle Getting the highest and lowest temperature for the given day
handleCommand (MinMaxWeatherCmd args) = do
  result <- runExceptT $ handleWeather args
  case result of
    Left err -> print err
    Right (wData, weather) ->
      case date weather of
        Just d -> do
          res <- runExceptT $ handleMinMax wData d weather
          case res of
            Left e -> print e
            Right minMax -> putStrLn $ T.unpack minMax
        Nothing -> print $ MissingVal "Unable to fetch the appropriate date"

-- \| Handle mistyped/non-existing commands
handleCommand _ = print $ UnknownCommand "Something went wrong with the given command"

-- | Check if the timeseries is older than the given time
checkValidTime :: TimeSeries -> UTCTime -> Bool
checkValidTime ts inpDate =
  case ts ^. time of
    Just date' ->
      let time' = utctDayTime date'
          inpTime' = utctDayTime inpDate
       in inpTime' <= time'
    Nothing -> False

getMax :: [TimeSeries] -> UTCTime -> Maybe (TimeSeries, Double)
getMax tsList inpDate =
  let entries =
        mapMaybe
          ( \ts ->
              if checkValidTime ts inpDate
                then do
                  timeData' <- ts ^. timeData
                  instant' <- timeData' ^. instant
                  details' <- instant' ^. detail
                  temp <- details' ^. airTemperature
                  return (ts, temp)
                else Nothing
          )
          tsList
   in foldl
        ( \maxT x ->
            case maxT of
              Nothing -> Just x
              Just (_, temp) -> if snd x >= temp then Just x else maxT
        )
        Nothing
        entries

getMin :: [TimeSeries] -> UTCTime -> Maybe (TimeSeries, Double)
getMin tsList inpDate =
  let entries =
        mapMaybe
          ( \ts ->
              if checkValidTime ts inpDate
                then do
                  timeData' <- ts ^. timeData
                  instant' <- timeData' ^. instant
                  details' <- instant' ^. detail
                  temp <- details' ^. airTemperature
                  return (ts, temp)
                else Nothing
          )
          tsList
   in foldl
        ( \minT x ->
            case minT of
              Nothing -> Just x
              Just (_, temp) -> if snd x <= temp then Just x else minT
        )
        Nothing
        entries

-- | Filter the weather data to get all TimeSeriesData for a given date
filterWeatherByDate :: WeatherData -> Day -> [TimeSeries]
filterWeatherByDate weatherData targetDate = do
  let props = fromJust (weatherData ^. properties)
  let tsList = fromJust (props ^. timeseries)
  filter (\ts -> utctDay (fromJust (ts ^. time)) == targetDate) tsList

-- | Filter the weatherdata to get only the timeseries data
-- TODO extend this to maybe have more complex data?
getSpecificTimeSeries :: WeatherData -> UTCTime -> Maybe TimeSeriesData
getSpecificTimeSeries w targetTime = do
  props <- w ^. properties
  tsList <- props ^. timeseries
  timeSeries <- find (\ts -> ts ^. time == Just targetTime) tsList
  timeSeries ^. timeData

--- TIME HELPER FUNCTIONS

-- | Get the current time and convert it according to the timezone
currentTime :: IO UTCTime
currentTime = do
  zonedTime <- getZonedTime
  currTime <- getCurrentTime
  let tZone = zonedTimeZone zonedTime -- Get timezone from the current time
      minutes = timeZoneMinutes tZone -- Get the minutes
      -- Add additional seconds to the utcTime given the current timezone
      time' = addUTCTime (fromIntegral (minutes * 60)) currTime
      roundToHour = roundToNearestHour time'
  return roundToHour

-- | Round the time to the nearest hour, since the weather data works on hours
roundToNearestHour :: UTCTime -> UTCTime
roundToNearestHour t =
  -- Extract Date
  let day = utctDay t
      seconds = round (utctDayTime t) :: Int
      (hours, rest) = seconds `divMod` 3600
      -- Round it up between the halfway point (<30 min = current hour, else +1 hour)
      roundedHours = if rest >= 1800 then hours + 1 else hours
      roundedHoursInSec = fromIntegral $ roundedHours * 3600
   in UTCTime day roundedHoursInSec


------------ DISPLAY

-- | Display the weather in the output
display :: WeatherData -> Weather -> Text
display weatherData w =
  T.pack (show w)
    <> T.pack "\n"
    <> "--------------------------------- \n"
    <> displayWeather weatherData (fromJust (date w))
    <> "\n"

-- TODO Use Errors instead of text? Maybe Either IO Text ErrorTypes OPTIMIZE THIS
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
             in T.pack "- WeatherCondition: " <> T.pack (show weather) <> T.pack "\n" <> T.pack (show details')

-- | Display the help menu
displayCommands :: Text
displayCommands =
  "Here are the available commands:\n\n"
    <> " !current: Display the current weather given a city.\n"
    <> " By default it is set to Norway, but can also give country and date to get more accurate weatherdata.\n"
    <> " The date needs to have the ISO-8601 format => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " !current City ?,Country\n\n"
    <> " !weather: Display the weather given a city, country and date.\n"
    <> " The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ \n"
    <> " !weather City, Country, Date\n\n"
    <> " !location: location City ?,Country\n\n"
    <> " !quit: Exit the application\n\n"
    <> " !help: Display all available commands"