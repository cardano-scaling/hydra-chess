{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Games.Run where

import Cardano.Binary (serialize')
import Control.Monad (forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Game.Chess (Chess)
import Game.Client (runClient)
import Game.Client.Console (inputParser, mkImpureIO)
import Games.Logging (findLogFile, withLogger)
import Games.Options (Options (..), hydraGamesInfo)
import Games.Run.Cardano (CardanoNode (..), withCardanoNode)
import Games.Run.Hydra (HydraNode (..), withHydraNode)
import Games.Server.Hydra (HydraParty (..), withHydraServer)
import Games.Server.IO (notifyChessEvent)
import Options.Applicative (execParser)
import System.IO (BufferMode (..), hSetBuffering, stdout)

run :: IO ()
run = do
  Options{cardanoNetwork, onlyCardano} <- execParser hydraGamesInfo
  hSetBuffering stdout NoBuffering
  logFile <- findLogFile cardanoNetwork
  withLogger logFile $ \logger ->
    withCardanoNode logger cardanoNetwork $ \cardano ->
      if onlyCardano
        then runCardanoClient
        else startServers cardano
 where
  runCardanoClient =
    forever (threadDelay 60_000_000)

  startServers cardano@CardanoNode{network} =
    withHydraNode cardano $ \HydraNode{hydraParty, hydraHost} -> do
      let party = HydraParty $ serialize' hydraParty
      withHydraServer network party hydraHost $ \server -> do
        putStrLn $ "Starting client for " <> show party <> " and host " <> show hydraHost
        runClient @Chess @_ @_ server notifyChessEvent (mkImpureIO inputParser)
