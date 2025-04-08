module Utils where

import Control.Lens ((^.))
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Time
  ( Day,
    UTCTime (..),
    ZonedTime (..),
    addUTCTime,
    getCurrentTime,
    getZonedTime,
  )
import Data.Time.LocalTime (TimeZone (..))
import Types

-- | Get the highest temperature given the date (won't be date that are older than the input)
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

-- | Get the lowest temperature given the date (won't be date that are older than the input)
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

-- | Check if the timeseries is older than the given time
checkValidTime :: TimeSeries -> UTCTime -> Bool
checkValidTime ts inpDate =
  case ts ^. time of
    Just date' ->
      let time' = utctDayTime date'
          inpTime' = utctDayTime inpDate
       in inpTime' <= time'
    Nothing -> False
