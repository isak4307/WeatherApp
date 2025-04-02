{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- TODO APGL Copyright CC-By-SA pga data jeg bruker, bruk src library for å enklere lage tester
TODO wrapper around UTCTIMe so that I can create my own show instance for time?
TODO Use Errors instead of text? Maybe Either IO Text ErrorTypes OPTIMIZE the displayWeather
TODO extend this to maybe have more complex data? in filterweather
TODO parse weather doesn't work for city,date but maybe that should be seperated?
TODO Current and Weather are very similar to each other
chcp 65001
TODO CERTAIN TEXT WOULD be lost/ not encoded properly with utf-8 if using show. Replace with unpack
TODO FIX the display command to include weather command -}
{-
  Credits:
          Geographical Location data (latitude and longitude) is gathered from Nominatim/OpenStreetMap https://nominatim.org/
          Weather data is based from MET Norway https://www.met.no/
-}
module Main where


import qualified Data.Text as T

import Types

import Parser
import Handler
main :: IO ()
main = do
  putStrLn "> Starting up the application, type help for the list of commands"
  loop

-- FOR TERMINAL USAGE 
loop :: IO ()
loop = do
  putStr ">"
  inp <- getLine
  let cmd = parseCommand (T.pack inp)
  case cmd of
    Left e -> do
      putStrLn $ "Error: " <> show e
      loop
    Right Quit -> putStrLn "Exiting Application"
    Right command -> do
      if command == Help
        then
          putStrLn $ T.unpack displayCommands
        else
          handleCommand command
      loop
