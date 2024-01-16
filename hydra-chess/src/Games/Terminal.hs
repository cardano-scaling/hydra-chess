{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Terminal where

import Data.Aeson (ToJSON (toJSON), Value (..), Object)
import Data.Aeson.KeyMap ((!?))
import Games.Logging (Logger (..))
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  setSGR, hCursorBackward,
 )
import System.IO (BufferMode (NoBuffering), hPutStr, hSetBuffering, stdout)

withTerminalFrontend :: Logger -> (Logger -> IO a) -> IO a
withTerminalFrontend Logger{logEntry} k = do
  hSetBuffering stdout NoBuffering
  k Logger{logEntry = notifyProgress logEntry}

notifyProgress :: (ToJSON a) => (a -> IO ()) -> a -> IO ()
notifyProgress fn a = do
  let value = toJSON a
  case value of
    Object kv ->
      case kv !? "tag" of
        -- Cardano startup
        Just "CardanoNodeLaunching" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "Cardano|Launching "
          setSGR [Reset]
          setSGR
            [ SetColor Foreground Dull Yellow
            ]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CardanoNodeSyncing" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout "Cardano|Syncing   "
          setSGR [Reset]
          setSGR
            [ SetColor Foreground Dull Red
            ]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CardanoNodeSyncedAt" -> do
          let percent = mkPercentSynced kv
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout $ "Cardano|Sync " <> percent <> "%"
          setSGR [Reset]
          setSGR
            [ SetColor Foreground Dull Red
            ]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CardanoNodeFullySynced" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Cardano|Synced    "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        -- Hydra startup
        Just "HydraNodeStarting" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "Hydra  |Starting  "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Yellow]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "HydraNodeStarted" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Hydra  |Started   "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        -- Game server startup
        Just "GameServerStarting" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "Chess  |Starting  "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Yellow]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "ConnectingToHydra" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout "Chess  |Connecting"
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Red]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "ConnectedToHydra" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Chess  |Connected "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        _ -> pure ()
    _ -> pure ()
  fn a

mkPercentSynced :: Object -> String
mkPercentSynced kv =
  case kv !? "percentSynced" of
    Just (Number num ) -> show num
    _other -> "??.?"
