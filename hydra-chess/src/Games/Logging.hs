{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Games.Logging where

import Control.Monad.Class.MonadTime (getCurrentTime)
import Data.Aeson (ToJSON, Value (Object), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Games.Cardano.Network (Network, networkDir)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (AppendMode), withFile, hFlush)

findLogFile :: Network -> IO FilePath
findLogFile network = do
  logDir <- getXdgDirectory XdgCache ("chess" </> networkDir network)
  createDirectoryIfMissing True logDir
  pure $ logDir </> "game.log"

data Logger = Logger {logEntry :: forall a. (ToJSON a) => a -> IO ()}

withLogger :: FilePath -> (Logger -> IO a) -> IO a
withLogger logFile k =
  withFile logFile AppendMode $ \handle ->
    k (mkLogger handle)

logWith :: ToJSON a => Logger -> a -> IO ()
logWith Logger{logEntry} a = logEntry a

mkLogger :: Handle -> Logger
mkLogger hdl =
  Logger
    { logEntry = \a -> do
        time <- getCurrentTime
        let logged =
              case toJSON a of
                Object o ->
                  Object $ o <> KeyMap.fromList [("timestamp", toJSON time)]
                other -> object ["timestamp" .= time, "log" .= other]
        BS.hPutStr hdl $ LBS.toStrict $ encode logged <> "\n"
        hFlush hdl
    }
