{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Run.Cardano where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (Exception, finally, throwIO)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadTimer (threadDelay)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, encode)
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Games.Cardano.Network (Network, cardanoNodeVersion, networkDir, networkMagicArgs)
import Games.Logging (Logger (..), logWith)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (
  Permissions (..),
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesFileExist,
  getPermissions,
  getXdgDirectory,
  removeFile,
  setOwnerExecutable,
  setPermissions,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, withFile)
import qualified System.Info as System
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, readProcess, terminateProcess, waitForProcess, withCreateProcess)

data CardanoNode = CardanoNode
  { nodeSocket :: FilePath
  , network :: Network
  }
  deriving (Show)

data CardanoLog
  = CardanoNodeLaunching
  | CardanoNodeSyncing
  | CardanoNodeFullySynced
  | CardanoNodeSyncedAt {percentSynced :: Double}
  | DownloadingExecutables {downloadUrl :: String}
  | DownloadedExecutables { destination :: FilePath }
  | RetrievedConfigFile { configFile :: FilePath }
  | WaitingForNodeSocket {socketPath :: FilePath}
  | LoggingTo {logFile :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withCardanoNode :: Logger -> Network -> (CardanoNode -> IO a) -> IO a
withCardanoNode logger network k =
  withLogFile logger ("cardano-node" </> networkDir network) $ \out -> do
    exe <- findCardanoExecutable logger (cardanoNodeVersion network)
    socketPath <- findSocketPath network
    process <- cardanoNodeProcess logger exe network
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        ( race
            (checkProcessHasNotDied network "cardano-node" processHandle)
            (waitForNode socketPath k)
            >>= \case
              Left void -> absurd void
              Right a -> pure a
        )
          `finally` (cleanupSocketFile socketPath >> terminateProcess processHandle)
 where
  waitForNode socketPath cont = do
    let rn = CardanoNode{nodeSocket = socketPath, network}
    logWith logger CardanoNodeLaunching
    waitForSocket logger rn
    logWith logger CardanoNodeSyncing
    waitForFullSync logger rn
    logWith logger CardanoNodeFullySynced
    cont rn

  cleanupSocketFile socketPath = do
    exists <- doesFileExist socketPath
    when exists $ removeFile socketPath

findSocketPath :: Network -> IO FilePath
findSocketPath network = do
  socketDir <- getXdgDirectory XdgCache ("cardano-node" </> networkDir network)
  createDirectoryIfMissing True socketDir
  pure $ socketDir </> "node.socket"

findCardanoExecutable :: Logger -> String -> IO FilePath
findCardanoExecutable logger version = do
  let currentOS = System.os
  dataDir <- getXdgDirectory XdgData "cardano"
  createDirectoryIfMissing True dataDir
  let cardanoExecutable = dataDir </> "cardano-node"
  exists <- doesFileExist cardanoExecutable
  hasRightVersion <-
    if exists
      then (== version) <$> getVersion cardanoExecutable
      else pure True
  when (not exists || not hasRightVersion) $ do
    downloadCardanoExecutable logger version currentOS dataDir
    permissions <- getPermissions cardanoExecutable
    unless (executable permissions) $ setPermissions cardanoExecutable (setOwnerExecutable True permissions)
  pure cardanoExecutable

getVersion :: FilePath -> IO String
getVersion exe =
  readProcess exe ["--version"] "" >>= pure . takeWhile (/= ' ') . drop 13

findCardanoCliExecutable :: IO FilePath
findCardanoCliExecutable = do
  dataDir <- getXdgDirectory XdgData "cardano"
  let cardanoCliExecutable = dataDir </> "cardano-cli"
  permissions <- getPermissions cardanoCliExecutable
  unless (executable permissions) $ setPermissions cardanoCliExecutable (setOwnerExecutable True permissions)
  pure cardanoCliExecutable

downloadCardanoExecutable :: Logger -> String -> String -> FilePath -> IO ()
downloadCardanoExecutable logger version currentOs destDir = do
  let binariesUrl =
        "https://github.com/intersectMBO/cardano-node/releases/download/"
          <> version
          <> "-pre"
          <> "/cardano-node-"
          <> version
          <> "-"
          <> osTag currentOs
          <> ".tar.gz"
  request <- parseRequest $ "GET " <> binariesUrl
  logWith logger (DownloadingExecutables binariesUrl)
  httpLBS request >>= Tar.unpack destDir . Tar.read . GZip.decompress . getResponseBody
  logWith logger $ DownloadedExecutables destDir

osTag :: String -> String
osTag = \case
  "darwin" -> "macos"
  other -> other

-- | Wait for the node socket file to become available.
waitForSocket :: Logger -> CardanoNode -> IO ()
waitForSocket logger node@CardanoNode{nodeSocket} = do
  exists <- doesFileExist nodeSocket
  unless exists $ do
    logWith logger (WaitingForNodeSocket nodeSocket)
    threadDelay 1_000_000
    waitForSocket logger node

-- | Wait for the node to be fully synchronized.
waitForFullSync :: Logger -> CardanoNode -> IO ()
waitForFullSync logger node = do
  tip <- queryPercentSync node
  unless (tip == 100.0) $ do
    logWith logger (CardanoNodeSyncedAt tip)
    threadDelay 10_000_000
    waitForFullSync logger node

queryPercentSync :: CardanoNode -> IO Double
queryPercentSync CardanoNode{network} = do
  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network
  out <-
    (eitherDecode >=> extractSyncPercent) . Lazy.encodeUtf8 . LT.pack
      <$> readProcess cardanoCliExe (["query", "tip", "--socket-path", socketPath] <> networkMagicArgs network) ""
  either (throwIO . userError) pure out

extractSyncPercent :: Value -> Either String Double
extractSyncPercent = \case
  Object obj ->
    case obj !? "syncProgress" of
      Just (String txt) -> pure $ (read $ unpack txt)
      _ -> Left "Did not find 'syncProgress' field"
  v -> Left $ "query returned something odd: " <> LT.unpack (Lazy.decodeUtf8 $ encode v)

findConfigFiles :: Logger -> Network -> IO (FilePath, FilePath)
findConfigFiles logger network = do
  let nodeConfig = "config.json"
      nodeTopology = "topology.json"
      nodeByronGenesis = "byron-genesis.json"
      nodeShelleyGenesis = "shelley-genesis.json"
      nodeAlonzoGenesis = "alonzo-genesis.json"
      nodeConwayGenesis = "conway-genesis.json"
      envUrl = "https://book.world.dev.cardano.org/environments" </> networkDir network
  configDir <- getXdgDirectory XdgConfig ("cardano" </> networkDir network)
  createDirectoryIfMissing True configDir
  mapM_
    (retrieveConfigFile envUrl configDir)
    [ nodeConfig
    , nodeTopology
    , nodeByronGenesis
    , nodeShelleyGenesis
    , nodeAlonzoGenesis
    , nodeConwayGenesis
    ]
  pure (configDir </> nodeConfig, configDir </> nodeTopology)
 where
  retrieveConfigFile envUrl configDir config = do
    request <- parseRequest $ "GET " <> envUrl </> config
    let configFile = configDir </> config
    httpLBS request >>= LBS.writeFile configFile . getResponseBody
    logWith logger (RetrievedConfigFile configFile)

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Logger -> FilePath -> Network -> IO CreateProcess
cardanoNodeProcess logger exe network = do
  (nodeConfigFile, nodeTopologyFile) <- findConfigFiles logger network
  nodeDatabaseDir <- getXdgDirectory XdgData ("cardano" </> networkDir network)
  createDirectoryIfMissing True nodeDatabaseDir
  nodeSocket <- findSocketPath network
  pure $
    proc exe $
      "run"
        : [ "--config"
          , nodeConfigFile
          , "--topology"
          , nodeTopologyFile
          , "--database-path"
          , nodeDatabaseDir </> "db"
          , "--socket-path"
          , nodeSocket
          ]

withLogFile :: Logger -> String -> (Handle -> IO a) -> IO a
withLogFile logger namespace k = do
  logFile <- findLogFile namespace
  logWith logger (LoggingTo logFile)
  withFile logFile AppendMode (\out -> hSetBuffering out NoBuffering >> k out)

findLogFile :: String -> IO FilePath
findLogFile namespace = do
  logDir <- getXdgDirectory XdgCache namespace
  createDirectoryIfMissing True logDir
  pure $ logDir </> "node.log"

data ProcessExit
  = ProcessExited {processName :: String}
  | ProcessDied {processName :: String, exitCode :: Int, logFile :: FilePath}
  deriving stock (Show)

instance Exception ProcessExit

checkProcessHasNotDied :: Network -> String -> ProcessHandle -> IO Void
checkProcessHasNotDied network name processHandle = do
  logFile <- findLogFile (name </> networkDir network)
  waitForProcess processHandle >>= \case
    ExitSuccess -> throwIO (ProcessExited name)
    ExitFailure exit -> throwIO (ProcessDied name exit logFile)
