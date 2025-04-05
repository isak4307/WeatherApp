module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import Discord
import Handler
import Parser
import Types

main :: IO ()
main = do
  putStrLn "> Starting up the application, type !help for the list of available commands"
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
        else do
          result <- runExceptT $ handleCommand command
          case result of
            Left err -> print $ show err
            Right val -> putStrLn $ T.unpack val
      loop
