{-# LANGUAGE NamedFieldPuns #-}

module Games.Terminal where

import Games.Logging (Logger (..))

withTerminalFrontend :: Applicative m => Logger -> (Logger -> m a) -> m a
withTerminalFrontend Logger{logEntry} k =
  k Logger{logEntry = \a -> logEntry a}
