{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Text.Megaparsec
  ( anySingle,
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
  let cmd  = T.takeWhile (/= ' ') inp
      args = T.drop (T.length cmd + 1) inp
  case cmd of
    "!quit" -> Right Quit
    "!help" -> Right Help
    "!current" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _  -> Right $ CurrentWeatherCmd args
    "!weather" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city and date"
        _  -> Right $ WeatherCmd args
    "!location" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city"
        _  -> Right $ LocationCmd args
    "!minMax" ->
      case args of
        "" -> Left $ ArgumentError "No arguments provided, need at least city and date"
        _  -> Right $ MinMaxWeatherCmd args
    _ -> Left $ UnknownCommand $ "The following command: " <> cmd <> " is invalid"

-- | Parse the argument to create a Location datatype
parseGeoLocation :: Parser Location
parseGeoLocation =
  (Location . T.pack <$> some (satisfy (/= ',')))
    <*> optional
      ( char ','
          *> optional (char ' ')
          *> (T.pack <$> someTill anySingle eof)
      )

parseCurrentWeather :: Parser Weather
parseCurrentWeather =
  (Weather . T.pack <$> some (satisfy (/= ','))) -- Parse city
    <*> optional
      ( char ','
          *> optional (char ' ')
          *> (T.pack <$> some (satisfy (/= ',')))
      ) -- Parse country
    <*> pure Nothing


parseWeather :: Parser Weather
parseWeather =
  (Weather . T.pack <$> some (satisfy (/= ','))) -- Parse city
    <*> optional
      ( char ','
          *> optional (char ' ')
          *> (T.pack <$> some (satisfy (/= ',')))
      ) -- Parse country
    <*> ( do
            _ <- char ','
            _ <- optional (char ' ')
            dateText <- T.pack <$> someTill anySingle eof
            Just <$> parseDate dateText
        ) -- Parse date

-- | Convert the input text to the appropriate UTCTime value
parseDate :: Text -> Parser UTCTime
parseDate dateT = parseTimeM True defaultTimeLocale "%FT%T%QZ" (T.unpack dateT)
