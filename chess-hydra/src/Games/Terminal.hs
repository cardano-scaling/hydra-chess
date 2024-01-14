{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Terminal where

import Data.Aeson (ToJSON (toJSON), Value (..))
import Data.Aeson.KeyMap ((!?))
import Games.Logging (Logger (..))
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  setSGR,
 )
import System.IO (hPutStr, stdout)

withTerminalFrontend :: (Applicative m) => Logger -> (Logger -> m a) -> m a
withTerminalFrontend Logger{logEntry} k =
  k Logger{logEntry = notifyProgress logEntry}

notifyProgress :: (ToJSON a) => (a -> IO ()) -> a -> IO ()
notifyProgress fn a = do
  let value = toJSON a
  case value of
    Object kv ->
      case kv !? "tag" of
        Just "CardanoNodeLaunching" -> do
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "\ESC[1ECardano|Launching"
          setSGR [Reset]
          setSGR
            [ SetColor Foreground Dull Yellow
            ]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CardanoNodeSyncing" -> do
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout "\ESC[1ECardano|Syncing  "
          setSGR [Reset]
          setSGR
            [ SetColor Foreground Dull Red
            ]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CardanoNodeFullySynced" -> do
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Vivid Green
            ]
          hPutStr stdout "\ESC[1ECardano|Synced   "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Vivid Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        _ -> pure ()
    _ -> pure ()
  fn a
