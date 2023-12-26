{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Run.Hydra where

import Cardano.Binary (FromCBOR, fromCBOR, serialize')
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (SignKeyDSIGN),
  Ed25519DSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  hashVerKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.Hash (Blake2b_224)
import Cardano.Crypto.Seed (readSeedFromSystemEntropy)
import qualified Chess.ELO as ELO
import Chess.Plutus (MintAction (Mint), pubKeyHash)
import qualified Chess.Token as Token
import qualified Codec.Archive.Zip as Zip
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadTimer (threadDelay)
import Data.Aeson (Value (Number, String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap, insert, (!?))
import Data.Aeson.Types (Value (Object))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Data (Proxy (..))
import Data.Either (rights)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Game.Client.Console (Coins (..), SimpleUTxO (..), parseQueryUTxO)
import Game.Server (Host (..))
import Games.Run.Cardano (
  CardanoNode (..),
  Network (..),
  checkProcessHasNotDied,
  findCardanoCliExecutable,
  findSocketPath,
  networkDir,
  networkMagicArgs,
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
  { hydraParty :: VerKeyDSIGN Ed25519DSIGN
  , hydraHost :: Host
  }
  deriving (Show)

withHydraNode :: CardanoNode -> (HydraNode -> IO a) -> IO a
withHydraNode CardanoNode{network, nodeSocket} k =
  withLogFile ("hydra-node" </> networkDir network) $ \out -> do
    exe <- findHydraExecutable
    (me, process) <- hydraNodeProcess network exe nodeSocket
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        race
          (checkProcessHasNotDied network "hydra-node" processHandle)
          ( do
              putStrLn "Hydra node started"
              k (HydraNode me (Host "127.0.0.1" 34567))
          )
          >>= \case
            Left{} -> error "should never been reached"
            Right a -> pure a

findHydraScriptsTxId :: Network -> IO String
findHydraScriptsTxId = \case
  -- TODO: those should be pulled from some remotely published source
  -- FIXME: This is actually tied to the version
  Preview -> pure "64deee72cd424d957ea0fddf71508429ecb65fea83a041fe9b708fc2ca973a8e"
  Preprod -> pure "d8ba8c488f52228b200df48fe28305bc311d0507da2c2420b10835bf00d21948"
  Mainnet -> pure "3ac58d3f9f35d8f2cb38d39639232c10cfe0b986728f672d26ffced944d74560"

hydraNodeProcess :: Network -> FilePath -> FilePath -> IO (VerKeyDSIGN Ed25519DSIGN, CreateProcess)
hydraNodeProcess network executableFile nodeSocket = do
  (me, hydraSkFile) <- findHydraSigningKey network

  (cardanoSkFile, cardanoVkFile) <- findKeys Fuel network
  checkFundsAreAvailable network cardanoSkFile cardanoVkFile

  (gameSkFile, gameVkFile) <- findKeys Game network
  checkGameTokenIsAvailable network gameSkFile gameVkFile

  protocolParametersFile <- findProtocolParametersFile network
  hydraPersistenceDir <- findHydraPersistenceDir network
  hydraScriptsTxId <- findHydraScriptsTxId network
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
          <> networkMagicArgs network
      )
  pure (me, proc executableFile args)

checkGameTokenIsAvailable :: Network -> FilePath -> FilePath -> IO ()
checkGameTokenIsAvailable network gameSkFile gameVkFile = do
  (_, eloScriptFile) <- findEloScriptFile gameVkFile network
  eloScriptAddress <- getScriptAddress eloScriptFile network
  hasOutputAt network eloScriptAddress >>= \case
    Just utxo -> do
      putStrLn $ "Found game token " <> utxo <> " at address " <> show eloScriptAddress
      pure ()
    Nothing -> do
      putStrLn $ "No game token registered on " <> show network <> ", creating it"
      registerGameToken network gameSkFile gameVkFile
      waitForToken eloScriptAddress
 where
  waitForToken eloScriptAddress = do
    putStrLn $ "Wait for token creation tx"
    threadDelay 10_000_000
    hasOutputAt network eloScriptAddress
      >>= maybe (waitForToken eloScriptAddress) (const $ pure ())

hasOutputAt :: Network -> String -> IO (Maybe String)
hasOutputAt network address = do
  output <- getUTxOFor network address
  if (length output == 1)
    then pure $ Just $ head output
    else pure Nothing

getUTxOFor :: Network -> String -> IO [String]
getUTxOFor network address = do
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

registerGameToken :: Network -> FilePath -> FilePath -> IO ()
registerGameToken network gameSkFile gameVkFile = do
  (fundSk, fundVk) <- findKeys Fuel network
  (gameSk, gameVk) <- findKeys Game network
  fundAddress <- getVerificationKeyAddress fundVk network
  utxo <- getUTxOFor network fundAddress
  when (null utxo) $ error "No UTxO with funds"
  let txin = mkTxIn $ head utxo
  cardanoCliExe <- findCardanoCliExecutable

  socketPath <- findSocketPath network

  mintScriptFile <- findMintScriptFile network
  mintRedeemerFile <- findMintRedeermeFile network

  (pkh, eloScriptFile) <- findEloScriptFile gameVk network
  let eloDatumValue :: Integer = 1000
      eloDatumHash =
        Text.unpack $
          decodeUtf8 $
            Hex.encode $
              ELO.datumHashBytes eloDatumValue
  eloScriptAddress <- getScriptAddress eloScriptFile network

  txFileRaw <- mkstemp "tx.raw" >>= \(fp, hdl) -> hClose hdl >> pure fp

  let token = "1 " <> Token.validatorHashHex <.> pkh

      args =
        [ "transaction"
        , "build"
        , "--tx-in"
        , txin
        , "--tx-in-collateral"
        , txin
        , "--tx-out"
        , eloScriptAddress <> " + 1200000 lovelace + " <> token
        , -- 1.2 ADA is the minUTxO here :shrug:
          "--tx-out-datum-hash"
        , eloDatumHash
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

  putStrLn $ "Building transaction " <> (txFileRaw <.> "raw") <> " with arguments: " <> unwords args

  callProcess cardanoCliExe args

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

  putStrLn $ "Sign token creation tx " <> (txFileRaw <.> "signed")

  callProcess
    cardanoCliExe
    $ [ "transaction"
      , "submit"
      , "--tx-file"
      , txFileRaw <.> "signed"
      , "--socket-path"
      , socketPath
      ]
      <> networkMagicArgs network

  putStrLn $ "Submitted token creation tx"

findEloScriptFile :: FilePath -> Network -> IO (String, FilePath)
findEloScriptFile gameVkFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let eloScriptFile = configDir </> "elo-script.plutus"
  -- overwrite script every time?
  gameVk <- deserialiseFromEnvelope @(VerKeyDSIGN Ed25519DSIGN) gameVkFile
  let pkh = pubKeyHash $ hashVerKeyDSIGN @_ @Blake2b_224 gameVk
      bytes = ELO.validatorBytes pkh
  BS.writeFile eloScriptFile bytes
  pure (show pkh, eloScriptFile)

findMintRedeermeFile :: Network -> IO String
findMintRedeermeFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let redeemerScriptFile = configDir </> "chess-token-redeemer.json"
  BS.writeFile redeemerScriptFile $ Token.mintActionJSON Mint
  pure redeemerScriptFile

findMintScriptFile :: Network -> IO String
findMintScriptFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  let mintScriptFile = configDir </> "chess-token.plutus"
  -- always overwrite file with latest version?
  BS.writeFile mintScriptFile Token.validatorBytes
  pure mintScriptFile

mkTxIn :: String -> String
mkTxIn cliOutput = txId <> "#" <> txIx
 where
  txId = takeWhile (not . isSpace) cliOutput
  txIx = takeWhile isDigit $ dropWhile isSpace $ dropWhile isHexDigit $ cliOutput

checkFundsAreAvailable :: Network -> FilePath -> FilePath -> IO ()
checkFundsAreAvailable network signingKeyFile verificationKeyFile = do
  ownAddress <- getVerificationKeyAddress verificationKeyFile network
  output <- getUTxOFor network ownAddress
  let maxLovelaceAvailable = maximum $ fmap totalLovelace $ rights $ fmap (parseQueryUTxO . Text.pack) output
  when (maxLovelaceAvailable < 10_000_000) $ do
    putStrLn $
      "Hydra needs some funds to fuel the process, please ensure there's a UTxO with at least 10 ADAs at " <> ownAddress
    threadDelay 60_000_000
    checkFundsAreAvailable network signingKeyFile verificationKeyFile
 where
  totalLovelace :: SimpleUTxO -> Integer
  totalLovelace SimpleUTxO{coins = Coins{lovelace}} = lovelace

ed25519seedsize :: Word
ed25519seedsize = seedSizeDSIGN (Proxy @Ed25519DSIGN)

findHydraSigningKey :: Network -> IO (VerKeyDSIGN Ed25519DSIGN, FilePath)
findHydraSigningKey network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "hydra.sk"
  exists <- doesFileExist hydraSk
  if not exists
    then do
      seed <- readSeedFromSystemEntropy ed25519seedsize
      let sk = genKeyDSIGN @Ed25519DSIGN seed
          jsonEnvelope =
            object
              [ "type" .= ("HydraSigningKey_ed25519" :: Text)
              , "description" .= ("" :: Text)
              , "cborHex" .= decodeUtf8 (Hex.encode (serialize' sk))
              ]

      LBS.writeFile hydraSk (encode jsonEnvelope)
      pure (deriveVerKeyDSIGN sk, hydraSk)
    else do
      sk <- deserialiseFromEnvelope @(SignKeyDSIGN Ed25519DSIGN) hydraSk
      pure (deriveVerKeyDSIGN sk, hydraSk)

deserialiseFromEnvelope :: forall a. (FromCBOR a) => FilePath -> IO a
deserialiseFromEnvelope file = do
  envelope <- eitherDecode <$> LBS.readFile file
  case envelope of
    Right (Object val) -> do
      case val !? "cborHex" of
        Just (String str) ->
          case Hex.decode (encodeUtf8 str) of
            Right bs ->
              either
                (\err -> error $ "Failed to deserialised key " <> show bs <> " : " <> show err)
                (pure . snd)
                $ deserialiseFromBytes @a fromCBOR (LBS.fromStrict bs)
            Left err -> error $ "Failed to deserialised key " <> unpack str <> " : " <> err
        other -> error $ "Failed to deserialised key " <> show other
    other -> error $ "Failed to read Hydra key file " <> file <> ", " <> show other

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
    seed <- readSeedFromSystemEntropy (seedSizeDSIGN (Proxy @Ed25519DSIGN))
    let sk = genKeyDSIGN @Ed25519DSIGN seed
        jsonEnvelope =
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
      (\err -> error $ "Failed to extract protocol parameters: " <> show err)
      (LBS.writeFile hydraSk . encode)
      (mkZeroFeeParams <$> out)
  pure hydraSk

mkZeroFeeParams :: Value -> Value
mkZeroFeeParams = \case
  Object obj ->
    Object $
      insert "txFeeFixed" zero $
        insert "txFeePerByte" zero $
          updateExecutionPrices obj
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
                insert "pricesSteps" zero obj
          )
          m
      _ -> m

findHydraExecutable :: IO FilePath
findHydraExecutable = do
  dataDir <- getXdgDirectory XdgData "hydra"
  createDirectoryIfMissing True dataDir
  let hydraExecutable = dataDir </> "hydra-node"
  exists <- doesFileExist hydraExecutable
  unless exists $ downloadHydraExecutable dataDir
  permissions <- getPermissions hydraExecutable
  unless (executable permissions) $ setPermissions hydraExecutable (setOwnerExecutable True permissions)
  pure hydraExecutable

downloadHydraExecutable :: FilePath -> IO ()
downloadHydraExecutable destDir = do
  -- TODO: generalise URL when binaries are published
  let binariesUrl = "https://github.com/input-output-hk/hydra/releases/download/0.14.0/hydra-aarch64-darwin-0.14.0.zip"
  request <- parseRequest $ "GET " <> binariesUrl
  putStr "Downloading hydra executables"
  httpLBS request >>= Zip.extractFilesFromArchive [Zip.OptDestination destDir] . Zip.toArchive . getResponseBody
  putStrLn " done"

-- transction to burn a token
-- % cardano-cli transaction build
-- --tx-in 64604aaf0b43037708029d01031a0c5c96a82be3c6d77863ae855153d318327f#0
-- --tx-in a07ebd4cda2a8a8f235487f144e6b076148a3868bad5faae8aaa644132e26b7b#1
-- --mint '-1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f'
-- --mint-script-file ~/.config/hydra-node/preview/chess-token.plutus
-- --mint-redeemer-file ~/.config/hydra-node/preview/chess-token-redeemer.json
-- --change-address addr_test1vqdd0j63e83dvfgtmqpetfwfyrm0v29dcjcm6pt6s8ymaxqr3z3f6
-- --tx-in-collateral a07ebd4cda2a8a8f235487f144e6b076148a3868bad5faae8aaa644132e26b7b#1
-- --out-file tx.raw
