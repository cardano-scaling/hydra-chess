{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Game.Client where

import Control.Monad (forM_, unless)
import Control.Monad.Class.MonadAsync (MonadAsync, race_)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Functor (void, (<&>))
import Data.Text (Text)
import Game.Client.Console (helpText)
import Game.Client.IO (Command (..), Err (..), HasIO (..), Output (Bye, Ko, Ok))
import Game.Server (FromChain, Game (readPlay), HeadId (HeadId), Indexed (..), IsChain (..), Server (..))
import qualified Game.Server as Server

data Result c
  = TableCreated {parties :: [Party c], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Coin c, tableId :: Text}
  | TableFundingFailed {failureReason :: Text}

deriving instance (IsChain c) => Eq (Result c)
deriving instance (IsChain c) => Show (Result c)

runClient ::
  (Game g, IsChain c, MonadAsync m, MonadDelay m) =>
  Server g c m ->
  (FromChain g c -> m ()) ->
  HasIO Command Output m ->
  m ()
runClient server notifyEvent io = race_ (loop (handleCommand server) io) (notify 0)
 where
  notify fromIndex = do
    Indexed{lastIndex, events} <- poll server fromIndex (fromIndex + 10)
    unless (null events) $ forM_ events notifyEvent
    let newIndex = fromIndex + fromIntegral (length events)
    unless (lastIndex > newIndex) $ threadDelay 2000000
    notify newIndex

loop :: (Monad m) => (Command -> m Output) -> HasIO Command Output m -> m ()
loop handle io = do
  prompt io
  input io >>= \case
    Left EOF -> pure ()
    Left (Err err) -> output io (Ko err) >> loop handle io
    Right Quit -> void (output io Bye)
    Right cmd -> handle cmd >>= output io >> loop handle io

handleCommand :: forall g c m. (Game g, IsChain c, Monad m) => Server g c m -> Command -> m Output
handleCommand Server{initHead, play, closeHead, newGame, getConfiguration} = \case
  Help ->
    pure $ Ok helpText
  NewTable peers ->
    initHead peers <&> (\HeadId{headId} -> Ok . ("head initialised with id " <>) $ headId)
  Play p ->
    case readPlay @g p of
      Nothing -> pure (Ko $ "Invalid play " <> p)
      Just pl -> play pl >> pure (Ok "played")
  NewGame ->
    newGame >> pure (Ok "new game")
  Stop ->
    closeHead >> pure (Ok "closed")
  Config ->
    getConfiguration <&> Ok
  Quit -> pure Bye
