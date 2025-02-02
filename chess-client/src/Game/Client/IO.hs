{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Game.Client.IO where

import Control.Monad.State (MonadState, State, StateT, execState, gets, modify, runState, runStateT)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (Value)
import Data.Bifunctor (second)
import Data.Text (Text, pack)
import Data.Void (Void)
import Game.Server (GamePlay)
import Prelude hiding (getLine, print)

data Command
  = NewTable [Text]
  | Help
  | Play Text
  | NewGame
  | Stop
  | -- | Display internal information about the current configuration
    Config
  | Quit
  deriving stock (Eq, Show)

data Output
  = Bye
  | Ok Text
  | Ko Text
  deriving (Eq, Show)

data Err = EOF | Err Text
  deriving (Eq, Show)

data HasIO command output m = HasIO
  { output :: output -> m ()
  , input :: m (Either Err command)
  , problem :: output -> m ()
  , prompt :: m ()
  , exit :: m ()
  }

-- * Pure IO

data PureIO command output = PureIO
  { inputText :: [command]
  , outputText :: [output]
  , errorText :: [output]
  }

mkPureIO :: (MonadState (PureIO command output) m) => HasIO command output m
mkPureIO =
  HasIO
    { input = do
        ins <- gets inputText
        case ins of
          [] -> pure $ Left EOF
          (t : ts) -> modify (\e -> e{inputText = ts}) >> pure (Right t)
    , output = \t -> modify $ \e -> e{outputText = t : outputText e}
    , problem = \o -> modify $ \e -> e{errorText = o : errorText e}
    , prompt = pure ()
    , exit = pure ()
    }

withInput ::
  (Monad m) =>
  [command] ->
  StateT (PureIO command output) m a ->
  m (a, [output])
withInput stream act =
  second outputText <$> runStateT act (PureIO stream [] [])
