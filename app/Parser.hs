{-# LANGUAGE OverloadedStrings #-}

-- \|
-- Module      : Parser
-- Description : Contains all the parser functions for all the datatypes that need to be parsed.
module Parser where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Text.Megaparsec
  ( MonadParsec (try),
    anySingle,
    eof,
    optional,
    satisfy,
    some,
    someTill,
  )
import Text.Megaparsec.Char (char)
import Types

-- | Parse commands by seperating between the command and the arguments
parseCommand :: Text -> Either PError Cmd
parseCommand "" = Left EmptyInput
parseCommand inp = do
  let cmd = T.takeWhile (/= ' ') inp
      args = T.drop (T.length cmd + 1) inp
  case cmd of
    "!quit" -> Right Quit
    "!help" -> Right Help
    "!weather" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _ -> Right $ WeatherCmd args
    "!location" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _ -> Right $ LocationCmd args
    "!minMax" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _ -> Right $ MinMaxWeatherCmd args
    "!week" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _ -> Right $ WeekWeatherCmd args
    "!sun" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city and date"
        _ -> Right $ SunCmd args
    _ -> Left $ UnknownCommand $ "The following command: " <> cmd <> " is invalid"

-- | Parse the argument to create a Location datatype
parseGeoLocation :: Parser Location
parseGeoLocation =
  (Location . T.pack <$> some (satisfy (/= ','))) -- parse city
    <*> optional
      ( char ','
          *> optional (char ' ')
          *> (T.pack <$> someTill anySingle eof)
      ) -- parse country

-- | Parse the argument to create a Weather datatype
-- To seperate between country and date, I have to use (not isDigit) Parser assumes there are no digits in countries
parseWeather :: Parser Weather
parseWeather =
  (Weather . T.pack <$> some (satisfy (/= ','))) -- Parse city
    <*> ( optional (char ',')
            *> optional (char ' ')
            *> optional (try $ T.pack <$> some (satisfy (\c -> c /= ',' && not (isDigit c)))) -- Parse country
        )
    <*> ( do
            _ <- optional (char ',')
            _ <- optional (char ' ')
            dateText <- optional $ T.pack <$> someTill anySingle eof -- Parse the date in text
            case dateText of
              Nothing -> pure Nothing
              Just d -> pure <$> parseDate d
        )

-- | Convert the date text to the appropriate UTCTime format
parseDate :: Text -> Parser UTCTime
parseDate dateT = do parseTimeM True defaultTimeLocale "%FT%T%QZ" (T.unpack dateT)

-- | Parse the input to create a sunrise and sunset type
parseSun :: Parser Sun
parseSun =
  (Sun . T.pack <$> some (satisfy (/= ','))) -- Parse city
    <*> ( optional (char ',')
            *> optional (char ' ')
            *> optional (try $ T.pack <$> some (satisfy (\c -> c /= ',' && not (isDigit c)))) -- Parse maybe country
        )
    <*> ( do
            _ <- optional (char ',')
            _ <- optional (char ' ')
            T.pack <$> someTill anySingle eof
        ) -- Parse date