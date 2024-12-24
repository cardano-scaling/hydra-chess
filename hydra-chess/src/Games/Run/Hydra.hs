{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Run.Hydra where

import Cardano.Binary (FromCBOR, fromCBOR, serialize')
import qualified Chess.Contract as Contract
import Chess.Data (
  datumJSON,
 )
import qualified Chess.ELO as ELO
import Chess.Plutus (
  MintAction (Mint),
  ToData,
  currencyFromBytes,
  pubKeyHash,
  scriptHashToBytes,
  validatorToBytes,
 )
import qualified Chess.Token as Token
import qualified Codec.Archive.Zip as Zip
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadTimer (threadDelay)
import Crypto.Hash (Blake2b_224, hash)
import Crypto.PubKey.Curve25519 (PublicKey, SecretKey, generateSecretKey, toPublic)
import Data.Aeson (
  FromJSON,
  ToJSON,
  Value (Number, String),
  decodeFileStrict',
  eitherDecode,
  encode,
  object,
  (.:),
  (.=),
 )
import Data.Aeson.KeyMap (KeyMap, insert, (!?))
import Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (Object))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Function (on)
import qualified Data.List as List
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Data.Void (absurd)
import GHC.Generics (Generic)
import Game.Client.Console (Coins (..))
import Game.Server (Host (..))
import Games.Cardano.Crypto ()
import Games.Cardano.Network (Network (..), networkDir, networkMagicArgs)
import Games.Logging (Logger, logWith)
import Games.Run.Cardano (
  CardanoLog (DownloadedExecutables, DownloadingExecutables),
  CardanoNode (..),
  checkProcessHasNotDied,
  findCardanoCliExecutable,
  findSocketPath,
  withLogFile,
 )
import Games.Server.JSON (FullUTxO (..), UTxOs (..), valueContains)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (
  Permissions (..),
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesFileExist,
  getPermissions,
  getXdgDirectory,
  setOwnerExecutable,
  setPermissions,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO (hClose)
import qualified System.Info as System
import System.Posix (mkstemp)
import System.Process (
  CmdSpec (..),
  CreateProcess (..),
  StdStream (..),
  callProcess,
  proc,
  readProcess,
  withCreateProcess,
 )

data HydraNode = HydraNode
  { hydraParty :: PublicKey
  , hydraHost :: Host
  }
  deriving (Show)

version :: String
version = "0.20.0"

data HydraLog
  = HydraNodeStarting
  | CheckingGameToken {publicKeyHash :: String, address :: String}
  | NoGameTokenRegistered {address :: String, network :: Network}
  | GameTokenRegistered {address :: String, network :: Network}
  | WaitForTokenRegistration {token :: String}
  | QueryingUtxo {address :: String}
  | BuildingTransaction {file :: FilePath, args :: [String]}
  | CardanoCliOutput {file :: FilePath, output :: String}
  | CardanoCliResult {file :: FilePath, result :: (ExitCode, String, String)}
  | SubmittedTransaction {file :: FilePath}
  | ErrorSubmittingTransaction {reason :: String}
  | UsingPeersFile {file :: FilePath}
  | NoPeersDefined
  | CheckingHydraFunds {address :: String}
  | NotEnoughFundsForHydra {network :: Network, address :: String, available :: Integer}
  | CheckedFundForHydra {address :: String}
  | HydraNodeStarted {cmdspec :: CmdSpec}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSON ExitCode where
  toJSON = \case
    ExitSuccess -> Number 0
    ExitFailure n -> Number $ fromIntegral n

instance FromJSON ExitCode where
  parseJSON = \case
    Number 0 -> pure ExitSuccess
    Number n -> pure $ ExitFailure $ floor n
    other -> fail $ "Cannot parse ExitCode, expected number, got " <> show other

instance ToJSON CmdSpec where
  toJSON = \case
    ShellCommand cmd ->
      String $ Text.pack cmd
    RawCommand exe args ->
      object
        [ "exe" .= exe
        , "args" .= args
        ]

instance FromJSON CmdSpec where
  parseJSON = \case
    String cmd -> pure $ ShellCommand $ Text.unpack cmd
    Object kv -> do
      exe <- kv .: "exe"
      args <- kv .: "args"
      pure $ RawCommand exe args
    other -> fail $ "Cannot parse CmdSpec, expected string or object, got " <> show other

withHydraNode :: Logger -> CardanoNode -> (HydraNode -> IO a) -> IO a
withHydraNode logger CardanoNode{network, nodeSocket} k =
  withLogFile logger ("hydra-node" </> networkDir network) $ \out -> do
    logWith logger HydraNodeStarting
    exe <- findHydraExecutable logger
    (me, process) <- hydraNodeProcess logger network exe nodeSocket
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        race
          (checkProcessHasNotDied network "hydra-node" processHandle)
          ( do
              let CreateProcess{cmdspec} = process
              logWith logger $ HydraNodeStarted cmdspec
              k (HydraNode me (Host "127.0.0.1" 34567))
          )
          >>= \case
            Left void -> absurd void
            Right a -> pure a

findHydraScriptsTxId :: Network -> IO String
findHydraScriptsTxId = \case
  -- TODO: use https://raw.githubusercontent.com/input-output-hk/hydra/0.14.0/networks.json
  -- FIXME: This is actually tied to the version
  Preview -> pure "0fd2468a66a0b1cb944cff9512ecfa25cdd2799cb48b07210c449a5ecace267d"
  Preprod -> pure "7d2793a5b609d49876707c0f9fadeab1df77fe3cd964bd0c0f8ebe1f6ffad39f"
  Mainnet -> pure "ab1d9f8cca896bca06b70df74860deecf20774e03d8562aecaed37525f6ebead"

hydraNodeProcess :: Logger -> Network -> FilePath -> FilePath -> IO (PublicKey, CreateProcess)
hydraNodeProcess logger network executableFile nodeSocket = do
  (me, hydraSkFile) <- findHydraSigningKey logger network

  (cardanoSkFile, cardanoVkFile) <- findKeys Fuel network
  checkFundsAreAvailable logger network cardanoSkFile cardanoVkFile

  protocolParametersFile <- findProtocolParametersFile network
  hydraPersistenceDir <- findHydraPersistenceDir network
  hydraScriptsTxId <- findHydraScriptsTxId network

  -- peers
  peers <- findPeers logger network
  peerArguments <- concat <$> mapM (peerArgument network) peers

  let
    nodeId = "hydra"
    hydraPort :: Int = 5551
    apiPort :: Int = 34567
    monitoringPort :: Int = 6001
    args =
      [ "--node-id"
      , nodeId
      , "--api-host"
      , "127.0.0.1"
      , "--api-port"
      , show apiPort
      , "--host"
      , "0.0.0.0"
      , "--port"
      , show hydraPort
      , "--monitoring-port"
      , show monitoringPort
      , "--persistence-dir"
      , hydraPersistenceDir
      , "--hydra-signing-key"
      , hydraSkFile
      , "--cardano-signing-key"
      , cardanoSkFile
      , "--ledger-protocol-parameters"
      , protocolParametersFile
      , "--hydra-scripts-tx-id"
      , hydraScriptsTxId
      , "--node-socket"
      , nodeSocket
      ]
        <> peerArguments
        <> networkMagicArgs network
  pure (me, proc executableFile args)

checkGameTokenIsAvailable :: Logger -> Network -> FilePath -> FilePath -> IO FullUTxO
checkGameTokenIsAvailable logger network gameSkFile gameVkFile = do
  pkh <- findPubKeyHash gameVkFile
  let token = "1 " <> Token.validatorHashHex <.> pkh

  eloScriptFile <- findEloScriptFile network
  eloScriptAddress <- getScriptAddress eloScriptFile network

  logWith logger $ CheckingGameToken pkh eloScriptAddress
  hasToken logger network token eloScriptAddress >>= \case
    Just utxo -> pure utxo
    Nothing -> do
      -- FIXME: it could be the case the token is already consumed in an ongoing game
      -- how to detect that situation? probably by wrapping the hydra server in such
      -- way that it's only started when the player wants to play, which means the
      -- controller knows there's an ongoing game it does not try to recreate a game
      -- token
      logWith logger (NoGameTokenRegistered eloScriptAddress network)
      registerGameToken logger network gameSkFile gameVkFile
      waitForToken token eloScriptAddress
 where
  waitForToken token eloScriptAddress = do
    logWith logger (WaitForTokenRegistration token)
    threadDelay 10_000_000
    hasToken logger network token eloScriptAddress
      >>= maybe
        (waitForToken token eloScriptAddress)
        ( \utxo -> do
            logWith logger $ GameTokenRegistered eloScriptAddress network
            pure utxo
        )

hasToken :: Logger -> Network -> String -> String -> IO (Maybe FullUTxO)
hasToken logger network token eloScriptAddress = do
  getUTxOFor logger network eloScriptAddress
    >>= pure . \case
      UTxOs [] -> Nothing
      UTxOs utxos ->
        case filter (valueContains $ Text.pack token) utxos of
          utxo : _ -> Just utxo -- FIXME: can there be multiple game tokens?
          [] -> Nothing

getUTxOFor :: Logger -> Network -> String -> IO UTxOs
getUTxOFor logger network address = do
  logWith logger $ QueryingUtxo address
  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network
  out <-
    readProcess
      cardanoCliExe
      ( [ "query"
        , "utxo"
        , "--output-json"
        , "--address"
        , address
        , "--socket-path"
        , socketPath
        ]
          <> networkMagicArgs network
      )
      ""
  case eitherDecode (Lazy.encodeUtf8 $ LT.pack out) of
    Right utxos -> pure utxos
    Left err -> throwIO $ userError $ "Failed to parse UTxO from " <> out <> "\nerror: " <> err

getVerificationKeyAddress :: FilePath -> Network -> IO String
getVerificationKeyAddress vkFile network = do
  cardanoCliExe <- findCardanoCliExecutable
  readProcess cardanoCliExe (["address", "build", "--verification-key-file", vkFile] <> networkMagicArgs network) ""

getScriptAddress :: FilePath -> Network -> IO String
getScriptAddress vkFile network = do
  cardanoCliExe <- findCardanoCliExecutable
  readProcess cardanoCliExe (["address", "build", "--payment-script-file", vkFile] <> networkMagicArgs network) ""

registerGameToken :: Logger -> Network -> FilePath -> FilePath -> IO ()
registerGameToken logger network gameSkFile gameVkFile = do
  (fundSk, fundVk) <- findKeys Fuel network
  (_, gameVk) <- findKeys Game network

  fundAddress <- getVerificationKeyAddress fundVk network
  eloScriptFile <- findEloScriptFile network
  eloScriptAddress <- getScriptAddress eloScriptFile network

  UTxOs utxo <- getUTxOFor logger network fundAddress -- TODO: check it has enough ADAs
  when (null utxo) $ throwIO (userError "No UTxO with funds")
  let txin =
        mkTxIn $
          List.maximumBy (compare `on` totalLovelace) utxo

  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network

  mintScriptFile <- findMintScriptFile network
  mintRedeemerFile <- findMintRedeermeFile network

  pkh <- findPubKeyHash gameVk

  txFileRaw <- mkTempFile

  let token = "1 " <> Token.validatorHashHex <.> pkh

      args =
        [ "conway"
        , "transaction"
        , "build"
        , "--tx-in"
        , txin
        , "--tx-in-collateral"
        , txin
        , "--tx-out"
        , eloScriptAddress <> " + 10000000 lovelace + " <> token
        , "--mint"
        , token
        , "--mint-script-file"
        , mintScriptFile
        , "--mint-redeemer-file"
        , mintRedeemerFile
        , "--change-address"
        , fundAddress
        , "--out-file"
        , txFileRaw <.> "raw"
        , "--socket-path"
        , socketPath
        ]
          <> networkMagicArgs network

  logWith logger $ BuildingTransaction (txFileRaw <.> "raw") args

  readProcess cardanoCliExe args [] >>= logWith logger . CardanoCliOutput cardanoCliExe

  callProcess
    cardanoCliExe
    $ [ "conway"
      , "transaction"
      , "sign"
      , "--signing-key-file"
      , fundSk
      , "--tx-file"
      , txFileRaw <.> "raw"
      , "--out-file"
      , txFileRaw <.> "signed"
      ]
      <> networkMagicArgs network

  readProcess
    cardanoCliExe
    ( [ "conway"
      , "transaction"
      , "submit"
      , "--tx-file"
      , txFileRaw <.> "signed"
      , "--socket-path"
      , socketPath
      ]
        <> networkMagicArgs network
    )
    []
    >>= logWith logger . CardanoCliOutput cardanoCliExe

  logWith logger $ SubmittedTransaction (txFileRaw <.> "signed")

findPubKeyHash :: FilePath -> IO String
findPubKeyHash vkFile =
  show . pubKeyHash . convert . hash @_ @Blake2b_224 <$> deserialiseFromEnvelope @PublicKey vkFile

findEloScriptFile :: Network -> IO FilePath
findEloScriptFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let eloScriptFile = configDir </> "elo-script.plutus"
  doesFileExist eloScriptFile >>= \case
    True -> pure ()
    False -> BS.writeFile eloScriptFile eloScriptBytes
  pure eloScriptFile

eloScriptBytes :: BS.ByteString
eloScriptBytes = ELO.validatorBytes (currencyFromBytes $ scriptHashToBytes Token.validatorHash)

findDatumFile :: (ToData a) => String -> a -> Network -> IO String
findDatumFile name datum network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let datumFile = configDir </> "chess-" <> name <.> "json"
  BS.writeFile datumFile $ datumJSON datum
  pure datumFile

findMintRedeermeFile :: Network -> IO String
findMintRedeermeFile = findDatumFile "mint-redeemer" Mint

findMintScriptFile :: Network -> IO String
findMintScriptFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let mintScriptFile = configDir </> "chess-token.plutus"
  -- always overwrite file with latest version?
  BS.writeFile mintScriptFile Token.validatorBytes
  pure mintScriptFile

findGameScriptFile :: Network -> IO FilePath
findGameScriptFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let gameScriptFile = configDir </> "game-script.plutus"
  BS.writeFile gameScriptFile (validatorToBytes Contract.validatorScript)
  pure gameScriptFile

makeGameFlatFile :: Network -> IO FilePath
makeGameFlatFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let gameFlatFile = configDir </> "game-script.flat"
  BS.writeFile gameFlatFile (SBS.fromShort Contract.validatorScript)
  pure gameFlatFile

mkTxIn :: FullUTxO -> String
mkTxIn FullUTxO{txIn} = Text.unpack txIn

checkFundsAreAvailable :: Logger -> Network -> FilePath -> FilePath -> IO ()
checkFundsAreAvailable logger network signingKeyFile verificationKeyFile = do
  ownAddress <- getVerificationKeyAddress verificationKeyFile network
  logWith logger $ CheckingHydraFunds ownAddress
  UTxOs output <- getUTxOFor logger network ownAddress
  let maxLovelaceAvailable =
        if null output
          then 0
          else maximum $ fmap totalLovelace output
  when (maxLovelaceAvailable < 10_000_000) $ do
    logWith logger $ NotEnoughFundsForHydra network ownAddress maxLovelaceAvailable
    threadDelay 60_000_000
    checkFundsAreAvailable logger network signingKeyFile verificationKeyFile
  logWith logger $ CheckedFundForHydra ownAddress

totalLovelace :: FullUTxO -> Integer
totalLovelace FullUTxO{value = Coins{lovelace}} = lovelace

findHydraSigningKey :: Logger -> Network -> IO (PublicKey, FilePath)
findHydraSigningKey logger network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "hydra.sk"
  exists <- doesFileExist hydraSk
  unless exists $ do
    exe <- findHydraExecutable logger
    callProcess exe ["gen-hydra-key", "--output-file", configDir </> "hydra"]

  sk <- deserialiseFromEnvelope @SecretKey hydraSk
  pure (toPublic sk, hydraSk)

deserialiseFromEnvelope :: forall a. (FromCBOR a) => FilePath -> IO a
deserialiseFromEnvelope file = do
  cborHex <- extractCBORHex file
  case Hex.decode (encodeUtf8 cborHex) of
    Right bs ->
      either
        (\err -> throwIO $ userError ("Failed to deserialised key from " <> show bs <> " : " <> show err))
        (pure . snd)
        $ deserialiseFromBytes @a fromCBOR (LBS.fromStrict bs)
    Left err -> throwIO $ userError ("Failed to deserialise key from " <> unpack cborHex <> " : " <> err)

extractCBORHex :: FilePath -> IO Text
extractCBORHex file = do
  envelope <- eitherDecode <$> LBS.readFile file
  case envelope of
    Right (Object val) -> do
      case val !? "cborHex" of
        Just (String str) -> pure str
        other -> error $ "Failed to find cborHex key " <> show other
    other -> error $ "Failed to read envelope file " <> file <> ", " <> show other

data KeyRole = Fuel | Game

signingKeyFilePath :: FilePath -> KeyRole -> FilePath
signingKeyFilePath dir = \case
  Fuel -> dir </> "cardano.sk"
  Game -> dir </> "game.sk"

verificationKeyFilePath :: FilePath -> KeyRole -> FilePath
verificationKeyFilePath dir = \case
  Fuel -> dir </> "cardano.vk"
  Game -> dir </> "game.vk"

findKeys :: KeyRole -> Network -> IO (FilePath, FilePath)
findKeys keyRole network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let signingKeyFile = signingKeyFilePath configDir keyRole
  exists <- doesFileExist signingKeyFile
  unless exists $ do
    sk <- generateSecretKey
    let jsonEnvelope =
          object
            [ "type" .= ("PaymentSigningKeyShelley_ed25519" :: Text)
            , "description" .= ("Payment Signing Key" :: Text)
            , "cborHex" .= decodeUtf8 (Hex.encode (serialize' sk))
            ]

    LBS.writeFile signingKeyFile (encode jsonEnvelope)

  let verificationKeyFile = verificationKeyFilePath configDir keyRole
  vkExists <- doesFileExist verificationKeyFile
  unless vkExists $ do
    cardanoCliExe <- findCardanoCliExecutable
    callProcess cardanoCliExe ["key", "verification-key", "--signing-key-file", signingKeyFile, "--verification-key-file", verificationKeyFile]

  pure (signingKeyFile, verificationKeyFile)

findHydraPersistenceDir :: Network -> IO FilePath
findHydraPersistenceDir network = do
  persistenceDir <- getXdgDirectory XdgCache ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True persistenceDir
  pure persistenceDir

findProtocolParametersFile :: Network -> IO FilePath
findProtocolParametersFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "protocol-parameters.json"
  exists <- doesFileExist hydraSk
  unless exists $ do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    out <-
      eitherDecode . Lazy.encodeUtf8 . LT.pack
        <$> readProcess cardanoCliExe (["query", "protocol-parameters", "--socket-path", socketPath] <> networkMagicArgs network) ""
    either
      (\err -> throwIO $ userError ("Failed to extract protocol parameters: " <> show err))
      (LBS.writeFile hydraSk . encode)
      (mkZeroFeeParams <$> out)
  pure hydraSk

mkZeroFeeParams :: Value -> Value
mkZeroFeeParams = \case
  Object obj ->
    Object $
      insert "utxoCostPerByte" zero $
        insert "txFeeFixed" zero $
          insert "txFeePerByte" zero $
            updateExecutionPrices $
              updateMaxTxExecutionUnits obj
  other -> other
 where
  zero = Number 0

  updateExecutionPrices :: KeyMap Value -> KeyMap Value
  updateExecutionPrices m =
    case m !? "executionUnitPrices" of
      Just (Object obj) ->
        insert
          "executionUnitPrices"
          ( Object $
              insert "pricesMemory" zero $
                insert "pricesSteps" zero $
                  insert "priceMemory" zero $
                    insert "priceSteps" zero obj
          )
          m
      _ -> m

  updateMaxTxExecutionUnits :: KeyMap Value -> KeyMap Value
  updateMaxTxExecutionUnits m =
    case m !? "maxTxExecutionUnits" of
      Just (Object obj) ->
        insert
          "maxTxExecutionUnits"
          ( Object $
              insert "memory" ten_billions $
                insert "steps" one_trillion obj
          )
          m
      _ -> m

  ten_billions = Number 10_000_000_000

  one_trillion = Number 1_000_000_000_000

findHydraExecutable :: Logger -> IO FilePath
findHydraExecutable logger = do
  dataDir <- getXdgDirectory XdgData "hydra"
  createDirectoryIfMissing True dataDir
  let hydraExecutable = dataDir </> "hydra-node"
  exists <- doesFileExist hydraExecutable
  unless exists $ downloadHydraExecutable logger dataDir
  permissions <- getPermissions hydraExecutable
  unless (executable permissions) $ setPermissions hydraExecutable (setOwnerExecutable True permissions)
  pure hydraExecutable

downloadHydraExecutable :: Logger -> FilePath -> IO ()
downloadHydraExecutable logger destDir = do
  let binariesUrl =
        "https://github.com/input-output-hk/hydra/releases/download/"
          <> version
          <> "/hydra-"
          <> System.arch
          <> "-"
          <> System.os
          <> "-"
          <> version
          <> ".zip"
  request <- parseRequest $ "GET " <> binariesUrl
  logWith logger $ DownloadingExecutables destDir binariesUrl
  httpLBS request >>= Zip.extractFilesFromArchive [Zip.OptDestination destDir] . Zip.toArchive . getResponseBody
  logWith logger $ DownloadedExecutables destDir

mkTempFile :: IO FilePath
mkTempFile = mkstemp "tx.raw." >>= \(fp, hdl) -> hClose hdl >> pure fp

data Peer = Peer
  { name :: String
  -- ^ This peer identifier, must be unique across all peers
  , cardanoKey :: Text
  -- ^ Hex encoded CBOR serialisation of an Ed25519 Cardano VK
  , hydraKey :: Text
  -- ^ Hex encoded CBOR serialisation of an Ed25519 Hydra VK
  , address :: Host
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

peerArgument :: Network -> Peer -> IO [String]
peerArgument network Peer{name, cardanoKey, hydraKey, address = Host{host, port}} = do
  -- we need to write files for the peer
  peersDir <- getXdgDirectory XdgCache ("hydra-node" </> networkDir network </> "peers")
  createDirectoryIfMissing True peersDir
  let cardanoVkEnvelope =
        object
          [ "type" .= ("PaymentVerificationKeyShelley_ed25519" :: Text)
          , "description" .= ("" :: Text)
          , "cborHex" .= cardanoKey
          ]
      cardanoKeyFile = peersDir </> name <.> "cardano" <.> "vk"
      hydraVkEnvelope =
        object
          [ "type" .= ("HydraVerificationKey_ed25519" :: Text)
          , "description" .= ("" :: Text)
          , "cborHex" .= hydraKey
          ]
      hydraKeyFile = peersDir </> name <.> "hydra" <.> "vk"

  doesFileExist cardanoKeyFile >>= \case
    False -> LBS.writeFile cardanoKeyFile (encode cardanoVkEnvelope)
    True -> pure ()

  doesFileExist hydraKeyFile >>= \case
    False -> LBS.writeFile hydraKeyFile (encode hydraVkEnvelope)
    True -> pure ()

  pure
    [ "--peer"
    , unpack host <> ":" <> show port
    , "--cardano-verification-key"
    , cardanoKeyFile
    , "--hydra-verification-key"
    , hydraKeyFile
    ]

-- TODO: should detect the peers configuration has changed and not reuse the same
-- state
findPeers :: Logger -> Network -> IO [Peer]
findPeers logger network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let peersFile = configDir </> "peers.json"
  exists <- doesFileExist peersFile
  if exists
    then do
      logWith logger $ UsingPeersFile peersFile
      maybe (throwIO $ userError $ "Failed to decode peers file " <> peersFile) pure =<< decodeFileStrict' peersFile
    else do
      logWith logger NoPeersDefined
      pure []
