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
import Options.Applicative (execParser)
import System.IO (BufferMode (..), hSetBuffering, stdout, stdin, hIsTerminalDevice)

run :: IO ()
run = do
  Options{cardanoNetwork, onlyCardano} <- execParser hydraGamesInfo
  frontend <- hIsTerminalDevice stdin >>= \case
    True -> pure mkTerminalFrontend
    False -> error "Not implemented"
  hSetBuffering stdout NoBuffering
  logFile <- findLogFile cardanoNetwork
  withLogger logFile $ \logger ->
    withCardanoNode logger cardanoNetwork $ \cardano ->
      if onlyCardano
        then runCardanoClient
        else startServers logger cardano frontend
 where
  runCardanoClient =
    forever (threadDelay 60_000_000)

  mkTerminalFrontend server =
    runClient server notifyChessEvent (mkImpureIO inputParser)

  startServers logger cardano@CardanoNode{network} frontend =
    withHydraNode logger cardano $ \HydraNode{hydraParty, hydraHost} -> do
      let party = HydraParty $ serialize' hydraParty
      withHydraServer logger network party hydraHost $ \server -> do
        putStrLn $ "Starting client for " <> show party <> " and host " <> show hydraHost
        frontend server
