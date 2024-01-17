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
  pubKeyHash,
  pubKeyHashFromHex,
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
  (.=),
 )
import Data.Aeson.KeyMap (KeyMap, insert, (!?))
import Data.Aeson.Types (Value (Object))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Either (rights)
import Data.Function (on)
import qualified Data.List as List
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Data.Void (absurd)
import GHC.Generics (Generic)
import Game.Client.Console (Coins (..), SimpleUTxO (..), parseQueryUTxO)
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
import System.FilePath ((<.>), (</>))
import System.IO (hClose)
import qualified System.Info as System
import System.Posix (mkstemp)
import System.Process (
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
version = "0.14.0"

data HydraLog
  = HydraNodeStarting
  | CheckingGameToken {publicKeyHash :: String, address :: String}
  | NoGameTokenRegistered {address :: String, network :: Network}
  | GameTokenRegistered {address :: String, network :: Network}
  | WaitForTokenRegistration {token :: String}
  | QueryingUtxo {address :: String}
  | BuildingTransaction {file :: FilePath, args :: [String]}
  | CardanoCliOutput {file :: FilePath, output :: String}
  | SubmittedTransaction {file :: FilePath}
  | UsingPeersFile {file :: FilePath}
  | NoPeersDefined
  | CheckingHydraFunds {address :: String}
  | NotEnoughFundsForHydra {network :: Network, address :: String}
  | CheckedFundForHydra {address :: String}
  | HydraNodeStarted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
              logWith logger HydraNodeStarted
              k (HydraNode me (Host "127.0.0.1" 34567))
          )
          >>= \case
            Left void -> absurd void
            Right a -> pure a

findHydraScriptsTxId :: Network -> IO String
findHydraScriptsTxId = \case
  -- TODO: use https://raw.githubusercontent.com/input-output-hk/hydra/0.14.0/networks.json
  -- FIXME: This is actually tied to the version
  Preview -> pure "64deee72cd424d957ea0fddf71508429ecb65fea83a041fe9b708fc2ca973a8e"
  Preprod -> pure "d8ba8c488f52228b200df48fe28305bc311d0507da2c2420b10835bf00d21948"
  Mainnet -> pure "3ac58d3f9f35d8f2cb38d39639232c10cfe0b986728f672d26ffced944d74560"

hydraNodeProcess :: Logger -> Network -> FilePath -> FilePath -> IO (PublicKey, CreateProcess)
hydraNodeProcess logger network executableFile nodeSocket = do
  (me, hydraSkFile) <- findHydraSigningKey logger network

  (cardanoSkFile, cardanoVkFile) <- findKeys Fuel network
  checkFundsAreAvailable logger network cardanoSkFile cardanoVkFile

  (gameSkFile, gameVkFile) <- findKeys Game network
  checkGameTokenIsAvailable logger network gameSkFile gameVkFile

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
      ( [ "--node-id"
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
      )
  pure (me, proc executableFile args)

checkGameTokenIsAvailable :: Logger -> Network -> FilePath -> FilePath -> IO ()
checkGameTokenIsAvailable logger network gameSkFile gameVkFile = do
  pkh <- findPubKeyHash gameVkFile
  let token = "1 " <> Token.validatorHashHex <.> pkh
  gameAddress <- getVerificationKeyAddress gameVkFile network
  logWith logger $ CheckingGameToken pkh gameAddress
  hasToken logger network token gameAddress >>= \case
    Just{} -> pure ()
    Nothing -> do
      -- FIXME: it could be the case the token is already consumed in an ongoing game
      -- how to detect that situation? probably by wrapping the hydra server in such
      -- way that it's only started when the player wants to play, which means the
      -- controller knows there's an ongoing game it does not try to recreate a game
      -- token
      logWith logger (NoGameTokenRegistered gameAddress network)
      registerGameToken logger network gameSkFile gameVkFile
      waitForToken token gameAddress
 where
  waitForToken token gameAddress = do
    logWith logger (WaitForTokenRegistration token)
    threadDelay 10_000_000
    hasToken logger network token gameAddress
      >>= maybe (waitForToken token gameAddress) (const $ logWith logger $ GameTokenRegistered gameAddress network)

hasToken :: Logger -> Network -> String -> String -> IO (Maybe String)
hasToken logger network token gameAddress = do
  getUTxOFor logger network gameAddress
    >>= pure . \case
      [] -> Nothing
      utxos ->
        case filter (token `List.isInfixOf`) utxos of
          utxo : _ -> Just utxo -- FIXME: can there be multiple game tokens?
          [] -> Nothing

hasOutputAt :: Logger -> Network -> String -> IO (Maybe String)
hasOutputAt logger network address = do
  output <- getUTxOFor logger network address
  if (length output == 1)
    then pure $ Just $ head output
    else pure Nothing

getUTxOFor :: Logger -> Network -> String -> IO [String]
getUTxOFor logger network address = do
  logWith logger $ QueryingUtxo address
  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network
  drop 2 . lines
    <$> readProcess
      cardanoCliExe
      ( [ "query"
        , "utxo"
        , "--address"
        , address
        , "--socket-path"
        , socketPath
        ]
          <> networkMagicArgs network
      )
      ""

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
  (gameSk, gameVk) <- findKeys Game network
  fundAddress <- getVerificationKeyAddress fundVk network
  gameAddress <- getVerificationKeyAddress gameVk network

  utxo <- getUTxOFor logger network fundAddress -- TODO: check it has enough ADAs
  when (null utxo) $ throwIO (userError "No UTxO with funds")
  let txin =
        mkTxIn $
          List.maximumBy (compare `on` totalLovelace) $
            rights $
              fmap (parseQueryUTxO . Text.pack) utxo

  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network

  mintScriptFile <- findMintScriptFile network
  mintRedeemerFile <- findMintRedeermeFile network

  pkh <- findPubKeyHash gameVk

  txFileRaw <- mkTempFile

  let token = "1 " <> Token.validatorHashHex <.> pkh

      args =
        [ "transaction"
        , "build"
        , "--tx-in"
        , txin
        , "--tx-in-collateral"
        , txin
        , "--tx-out"
        , gameAddress <> " + 10000000 lovelace + " <> token
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
    $ [ "transaction"
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
    ( [ "transaction"
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

findEloScriptFile :: FilePath -> Network -> IO (String, FilePath)
findEloScriptFile gameVkFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let eloScriptFile = configDir </> "elo-script.plutus"
  -- FIXME: overwrite script every time?
  gameVk <- deserialiseFromEnvelope @PublicKey gameVkFile
  let pkh = pubKeyHash $ convert $ hash @_ @Blake2b_224 gameVk
      bytes = ELO.validatorBytes pkh
  BS.writeFile eloScriptFile bytes
  pure (show pkh, eloScriptFile)

makeEloScriptFile :: String -> Network -> IO String
makeEloScriptFile pkh network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let eloScriptFile = configDir </> "elo-script-" <> pkh <.> "plutus"
      bytes = ELO.validatorBytes (pubKeyHashFromHex $ Text.pack pkh)

  BS.writeFile eloScriptFile bytes
  pure eloScriptFile

eloScriptBytes :: FilePath -> Network -> IO BS.ByteString
eloScriptBytes gameVkFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  gameVk <- deserialiseFromEnvelope @PublicKey gameVkFile
  let pkh = pubKeyHash $ convert $ hash @_ @Blake2b_224 gameVk
  pure $ ELO.validatorBytes pkh

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
  let eloScriptFile = configDir </> "game-script.plutus"
  BS.writeFile eloScriptFile (validatorToBytes Contract.validatorScript)
  pure eloScriptFile

mkTxIn :: SimpleUTxO -> String
mkTxIn =
  Text.unpack . \case
    SimpleUTxO{txIn} -> txIn
    UTxOWithDatum{txIn} -> txIn

checkFundsAreAvailable :: Logger -> Network -> FilePath -> FilePath -> IO ()
checkFundsAreAvailable logger network signingKeyFile verificationKeyFile = do
  ownAddress <- getVerificationKeyAddress verificationKeyFile network
  logWith logger $ CheckingHydraFunds ownAddress
  output <- getUTxOFor logger network ownAddress
  let maxLovelaceAvailable =
        if null output
          then 0
          else maximum $ fmap totalLovelace $ rights $ fmap (parseQueryUTxO . Text.pack) output
  when (maxLovelaceAvailable < 10_000_000) $ do
    logWith logger $ NotEnoughFundsForHydra network ownAddress
    threadDelay 60_000_000
    checkFundsAreAvailable logger network signingKeyFile verificationKeyFile
  logWith logger $ CheckedFundForHydra ownAddress

totalLovelace :: SimpleUTxO -> Integer
totalLovelace = \case
  SimpleUTxO{coins = Coins{lovelace}} -> lovelace
  UTxOWithDatum{coins = Coins{lovelace}} -> lovelace

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
  logWith logger $ DownloadingExecutables binariesUrl
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
