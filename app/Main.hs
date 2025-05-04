{-# LANGUAGE OverloadedStrings #-}

-- \|
-- Module      : Main
-- Description : The Main file that executes and runs the program.
module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Discord
import Display
import Handler
import Parser
import Types

main :: IO ()
main =
  -- Uncomment discord and comment out terminal if you want to run it through the discord bot
  -- discord
  terminal

-- FOR TERMINAL USAGE
terminal :: IO ()
terminal = do
  putStrLn "> Starting up the application, type !help for the list of available commands"
  loop

-- | Ensure that the program doesn't stop after one interaction
loop :: IO ()
loop = do
  putStr ">"
  inp <- getLine
  let cmd = parseCommand (T.pack inp)
  case cmd of
    Left e -> do
      putStrLn $ T.unpack $ "Error: " <> toText e
      loop
    Right Quit -> putStrLn "Exiting Application"
    Right command -> do
      if command == Help
        then
          putStrLn $ T.unpack displayCommands
        else do
          result <- runExceptT $ handleCommand command
          case result of
            Left err -> putStrLn $ T.unpack $ "Error: " <> toText err
            Right val -> putStrLn $ T.unpack val
      loop
