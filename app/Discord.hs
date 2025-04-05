{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Discord where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop
import Control.Monad
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time ()
import qualified Di
import DiPolysemy
import Handler
import Optics
import Parser (parseCommand)
import qualified Polysemy as P
import Types

-- | For Discord usage
discord :: IO ()
discord = do
  token' <- T.pack <$> readFile "token.txt"
  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      $ runBotIO (BotToken token') defaultIntents
      $ do
        react @'MessageCreateEvt $ \(msg', _usr, _member) -> do
          -- debug
          info @Text $ "Received message: " <> (msg' ^. #content)
          --
          let content' = msg' ^. #content
          when (T.head content' == '!') $ do
            let parseInp = parseCommand content'
            case parseInp of
              Left e -> do
                void $ tell @Text msg' ("Error: " <> T.pack (show e))
              Right Quit -> do
                void $ tell @Text msg' "Exiting Application"
              Right Help -> void $ tell @Text msg' displayCommands
              Right cmd -> do
                v <- P.embed $ runExceptT $ handleCommand cmd
                case v of
                  Left err' -> void $ tell @Text msg' (T.pack $ "Error: " <> show err')
                  Right res -> void $ tell @Text msg' res