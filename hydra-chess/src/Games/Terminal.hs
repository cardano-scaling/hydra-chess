{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Terminal where

import Data.Aeson (Object, ToJSON (toJSON), Value (..))
import Data.Aeson.KeyMap ((!?))
import Data.Text (unpack)
import Games.Logging (Logger (..))
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  hClearFromCursorToLineEnd,
  hCursorBackward,
  setSGR,
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
          hPutStr stdout "Cardano|Syncing    "
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
          hPutStr stdout "Cardano|Synced     "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        -- Hydra startup
        Just "HydraNodeStarting" -> do
          hCursorBackward stdout 1000
          hClearFromCursorToLineEnd stdout
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "Hydra  |Starting   "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Yellow]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "CheckingHydraFunds" -> do
          hCursorBackward stdout 1000
          hClearFromCursorToLineEnd stdout
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Yellow
            ]
          hPutStr stdout "Hydra  |Check Funds"
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Yellow]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "NotEnoughFundsForHydra" -> do
          let address = getAddress kv
              network = getNetwork kv
          hCursorBackward stdout 1000
          hClearFromCursorToLineEnd stdout
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout "Hydra  |No Funds   "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Red]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout $ "Send at least 10 ADAs to " <> address <> " on " <> network <> ", rechecking in 60s"
        Just "CheckedHydraFunds" -> do
          hCursorBackward stdout 1000
          hClearFromCursorToLineEnd stdout
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Hydra  |Funds OK   "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "HydraNodeStarted" -> do
          hCursorBackward stdout 1000
          hClearFromCursorToLineEnd stdout
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Hydra  |Started    "
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
          hPutStr stdout "Chess  |Starting   "
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
          hPutStr stdout "Chess  |Connecting "
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
          hPutStr stdout "Chess  |Connected  "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "ConnectedTo" -> do
          let peer = getPeer kv
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout $ ("Chess  |On  |" <> take 6 peer <> replicate (6 - length peer) ' ')
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "DisconnectedFrom" -> do
          let peer = getPeer kv
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout $ ("Chess  |Off |" <> take 6 peer <> replicate (6 - length peer) ' ')
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Red]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
        Just "HeadOpened" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Green
            ]
          hPutStr stdout "Chess  |Opened     "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Green]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        Just "HeadClosed" -> do
          hCursorBackward stdout 1000
          setSGR
            [ SetConsoleIntensity NormalIntensity
            , SetColor Foreground Vivid White
            , SetColor Background Dull Red
            ]
          hPutStr stdout "Chess  |Closed     "
          setSGR [Reset]
          setSGR
            [SetColor Foreground Dull Red]
          hPutStr stdout "\x25b6"
          setSGR [Reset]
          hPutStr stdout "\n"
        _ -> pure ()
    _ -> pure ()
  fn a

getPeer :: Object -> String
getPeer kv =
  case kv !? "peer" of
    Just (String txt) -> unpack txt
    _other -> " ?"

getAddress :: Object -> String
getAddress kv =
  case kv !? "address" of
    Just (String txt) -> unpack txt
    _other -> " ?"

getNetwork :: Object -> String
getNetwork kv =
  case kv !? "network" of
    Just (String txt) -> unpack txt
    _other -> " ?"

mkPercentSynced :: Object -> String
mkPercentSynced kv =
  case kv !? "percentSynced" of
    Just (Number num) -> show num
    _other -> "??.?"
