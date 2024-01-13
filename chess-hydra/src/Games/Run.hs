{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Games.Run where

import Cardano.Binary (serialize')
import Control.Monad (forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Game.Client (runClient)
import Game.Client.Console (inputParser, mkImpureIO)
import Games.Logging (findLogFile, withLogger)
import Games.Options (Options (..), hydraGamesInfo)
import Games.Run.Cardano (CardanoNode (..), withCardanoNode)
import Games.Run.Hydra (HydraNode (..), withHydraNode)
import Games.Server.Hydra (HydraParty (..), withHydraServer)
import Games.Server.IO (notifyChessEvent)
import Games.Terminal (withTerminalFrontend)
import Options.Applicative (execParser)
import System.IO (BufferMode (..), hIsTerminalDevice, hSetBuffering, stdin, stdout)

run :: IO ()
run = do
  Options{cardanoNetwork, onlyCardano} <- execParser hydraGamesInfo
  hSetBuffering stdout NoBuffering
  logFile <- findLogFile cardanoNetwork
  withLogger logFile $ \fileLogger -> do
    withFrontend <-
      hIsTerminalDevice stdin >>= \case
        True -> pure $ withTerminalFrontend fileLogger
        False -> error "Not implemented"
    withFrontend $ \logger ->
      withCardanoNode logger cardanoNetwork $ \cardano ->
        if onlyCardano
          then runCardanoClient
          else startServers logger cardano
 where
  runCardanoClient =
    forever (threadDelay 60_000_000)

  startServers logger cardano@CardanoNode{network} =
    withHydraNode logger cardano $ \HydraNode{hydraParty, hydraHost} -> do
      let party = HydraParty $ serialize' hydraParty
      withHydraServer logger network party hydraHost $ \server -> do
        putStrLn $ "Starting client for " <> show party <> " and host " <> show hydraHost
        runClient server notifyChessEvent (mkImpureIO inputParser)
