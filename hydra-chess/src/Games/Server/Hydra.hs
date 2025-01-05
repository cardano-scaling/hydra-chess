{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Server.Hydra where

import Chess.Data (integerFromDatum)
import Chess.Elo.Score (Result (..), eloGain)
import Chess.Game (Check (..), Side (..))
import qualified Chess.Game as Chess
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Plutus (pubKeyHash, pubKeyHashFromHex, pubKeyHashToHex)
import qualified Chess.Token as Token
import Control.Concurrent.Class.MonadSTM (
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
 )
import Control.Exception (Exception, IOException)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Class.MonadAsync (async, link, withAsync)
import Control.Monad.Class.MonadThrow (MonadCatch (catch), throwIO, try)
import Control.Monad.Class.MonadTime (UTCTime)
import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Crypto.Hash (Blake2b_224, hash)
import Crypto.PubKey.Curve25519 (PublicKey)
import Data.Aeson (
  FromJSON,
  Result (..),
  ToJSON (..),
  Value (..),
  decodeFileStrict,
  eitherDecode',
  encode,
  fromJSON,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (FromJSON (..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Word (Word64)
import GHC.Generics (Generic)
import Game.Chess (Chess, GamePlay (..))
import Game.Server (
  Content (..),
  FromChain (..),
  HeadId,
  Host (..),
  Indexed (..),
  IsChain (..),
  Server (..),
  ServerException (..),
 )
import Games.Cardano.Network (Network, networkMagicArgs)
import Games.Logging (Logger, logWith)
import Games.Run.Cardano (findCardanoCliExecutable, findSocketPath)
import Games.Run.Hydra (
  HydraLog (..),
  KeyRole (..),
  checkCollateralUTxO,
  checkGameTokenIsAvailable,
  deserialiseFromEnvelope,
  findDatumFile,
  findEloScriptFile,
  findGameScriptFile,
  findKeys,
  findProtocolParametersFile,
  findPubKeyHash,
  getScriptAddress,
  getUTxOFor,
  getVerificationKeyAddress,
  hasToken,
  mkTempFile,
 )
import Games.Server.JSON (
  Coin (..),
  Coins (..),
  FullUTxO (..),
  GameToken,
  GameTokens,
  UTxOs (..),
  asString,
  extractGameState,
  extractGameTxIn,
  hasAddress,
  stringifyValue,
 )
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestBodyJSON)
import Network.HTTP.Types (statusCode)
import Network.WebSockets (Connection, defaultConnectionOptions)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((<.>))
import System.IO (hClose)
import System.Posix (mkstemp)
import System.Process (callProcess, readProcessWithExitCode)
import Prelude hiding (seq)

-- | The type of backend provide by Hydra
data Hydra

-- | A hydra party is identified by its hydra verification key
data HydraParty = HydraParty {vkey :: ByteString}
  deriving (Eq)

instance Show HydraParty where
  show HydraParty{vkey} = unpack . decodeUtf8 . Hex.encode $ vkey

instance ToJSON HydraParty where
  toJSON HydraParty{vkey} =
    object ["vkey" .= (unpack . decodeUtf8 . Hex.encode $ vkey)]

instance FromJSON HydraParty where
  parseJSON = withObject "HydraParty" $ \obj ->
    (obj .: "vkey")
      >>= ( \case
              Left err -> fail err
              Right v -> pure $ HydraParty v
          )
        . Hex.decode
        . encodeUtf8

instance IsChain Hydra where
  type Party Hydra = HydraParty
  type Coin Hydra = ()

  partyId = pack . show . vkey

  coinValue = const 0

data CommitError = CommitError String
  deriving (Eq, Show, Exception)

instance ToJSON CommitError where
  toJSON (CommitError msg) =
    object ["tag" .= ("CommitError" :: Text), "error" .= msg]

data InvalidMove
  = InvalidMove Chess.IllegalMove
  | NoSingleOwnGameToken GameTokens GameTokens
  | UTxOError Text
  | NoGameFound
  deriving (Eq, Show, Exception)

data GameLog
  = GameServerStarting
  | ConnectingToHydra {host :: Text, port :: Int}
  | WaitingForConnectionToHydra {host :: Text, port :: Int}
  | ConnectedToHydra {host :: Text, port :: Int}
  | GameServerStarted
  | HydraCommandFailed {request :: Request}
  | ConnectedTo {peer :: Text}
  | DisconnectedFrom {peer :: Text}
  | NoGameToken {utxo :: UTxOs}
  | CommittingTo {headId :: HeadId}
  | WaitingForCommit {me :: HydraParty, headId :: HeadId}
  | CommittedTo {headId :: HeadId}
  | FoundGameTokens {own :: [GameToken], their :: [GameToken]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Configuration = Configuration
  { cardanoCliExe :: FilePath
  , socketPath :: FilePath
  , protocolParametersFile :: FilePath
  , gameScriptFile :: FilePath
  , gameScriptAddress :: Text
  , eloScriptFile :: FilePath
  , eloScriptAddress :: Text
  , fundVk :: FilePath
  , fundAddress :: Text
  , fundUTxO :: UTxOs
  , gameVk :: FilePath
  , gameAddress :: Text
  , gameUTxO :: UTxOs
  , ownToken :: Text
  , tokenUTxO :: UTxOs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventsQueue m e = EventsQueue
  { publish :: e -> m ()
  , toggleReplay :: m ()
  , isReplaying :: m Bool
  , readAll :: m (Seq e)
  , waitFor :: forall a. (e -> Maybe a) -> m a
  }

type ChessQueue = EventsQueue IO (FromChain Chess Hydra)

newEventsQueue :: IO (EventsQueue IO e)
newEventsQueue = do
  events <- newTVarIO mempty
  replaying <- newTVarIO True
  let publish e = atomically $ modifyTVar' events (|> e)
      toggleReplay = atomically $ modifyTVar' replaying not
      isReplaying = readTVarIO replaying
      readAll = readTVarIO events
  pure
    EventsQueue
      { waitFor = \predicate -> do
          atomically $ do
            readTVar events >>= \case
              -- FIXME: we can actually lose events, if 2 or more events are
              -- enqueued in the same transaction
              (_ :|> event) -> maybe retry pure (predicate event)
              _ -> retry
      , ..
      }

withHydraServer :: Logger -> Network -> HydraParty -> Host -> (Server Chess.Game Hydra IO -> IO ()) -> IO ()
withHydraServer logger network me host k = do
  logWith logger GameServerStarting
  queue <- newEventsQueue
  withClient logger host $ \cnx ->
    withAsync (pullEventsFromWs queue cnx) $ \thread ->
      let server =
            Server
              { initHead = sendInit queue cnx
              , poll = pollEvents queue
              , play = playGame queue cnx
              , newGame = newGame queue cnx
              , closeHead = sendClose queue cnx
              , getConfiguration
              }
       in do
            link thread
            logWith logger GameServerStarted
            k server
 where
  getConfiguration :: IO Text
  getConfiguration = do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    protocolParametersFile <- findProtocolParametersFile network
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- pack <$> getScriptAddress gameScriptFile network
    eloScriptFile <- findEloScriptFile network
    eloScriptAddress <- getScriptAddress eloScriptFile network

    (_, fundVk) <- findKeys Fuel network
    fundAddress <- getVerificationKeyAddress fundVk network
    fundUTxO <- getUTxOFor logger network fundAddress

    (_, gameVk) <- findKeys Game network
    gameAddress <- getVerificationKeyAddress gameVk network
    gameUTxO <- getUTxOFor logger network gameAddress

    pkh <- findPubKeyHash gameVk
    let ownToken = "1 " <> Token.validatorHashHex <.> pkh

    tokenUTxO <- UTxOs . maybeToList <$> hasToken logger network ownToken eloScriptAddress

    pure
      $ LT.toStrict
        . LT.decodeUtf8
      $ encodePretty
        Configuration
          { cardanoCliExe
          , socketPath
          , protocolParametersFile
          , gameScriptFile
          , gameScriptAddress
          , eloScriptFile
          , eloScriptAddress = pack eloScriptAddress
          , fundVk
          , fundAddress = pack fundAddress
          , fundUTxO
          , gameVk
          , gameAddress = pack gameAddress
          , gameUTxO
          , ownToken = pack ownToken
          , tokenUTxO
          }

  pollEvents :: ChessQueue -> Integer -> Integer -> IO (Indexed Chess Hydra)
  pollEvents queue (fromInteger -> start) (fromInteger -> count) = do
    history <- readAll queue
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length history
            , events = toList $ Seq.take count $ Seq.drop start history
            }
    pure indexed

  pullEventsFromWs :: ChessQueue -> Connection -> IO ()
  pullEventsFromWs queue cnx = do
    forever $
      WS.receiveData cnx >>= \dat -> do
        case eitherDecode' dat of
          Left err ->
            publish queue $ OtherMessage (Content $ pack err)
          Right (Response{output}) -> case output of
            HeadIsInitializing headId parties -> do
              publish queue $ HeadCreated headId parties
              replaying <- isReplaying queue
              unless replaying $ sendCommit queue cnx host 100 headId
            HeadIsAborted headId _ ->
              publish queue $ HeadClosed headId
            HeadIsFinalized headId _ ->
              publish queue $ HeadClosed headId
            Committed headId party _utxo ->
              publish queue $ FundCommitted headId party ()
            HeadIsOpen headId utxo -> do
              replaying <- isReplaying queue
              publish queue $ HeadOpened headId
            CommandFailed request ->
              logWith logger $ HydraCommandFailed request
            PostTxOnChainFailed{postTxError} ->
              publish queue $ OtherMessage (Content $ pack $ asString postTxError)
            Greetings{} ->
              -- the hydra server is ready to receive commands so this
              -- means we have stopped replay
              toggleReplay queue
            HeadIsClosed{} -> pure ()
            ReadyToFanout headId ->
              publish queue $ HeadClosing headId
            GetUTxOResponse{utxo} ->
              publish queue $ CollectUTxO (JsonContent $ toJSON utxo)
            RolledBack{} -> pure ()
            TxValid{} -> pure ()
            TxInvalid{validationError} ->
              publish queue $ OtherMessage (Content $ pack $ asString validationError)
            SnapshotConfirmed{headId, snapshot = Snapshot{utxo}} -> do
              replaying <- isReplaying queue
              handleGameState queue cnx headId utxo replaying
            PeerConnected{peer} ->
              logWith logger $ ConnectedTo peer
            PeerDisconnected{peer} ->
              logWith logger $ DisconnectedFrom peer
            InvalidInput{reason, input} ->
              publish queue $ OtherMessage (JsonContent $ toJSON output)
  -- TODO
  -- handle NotEnoughFuel message from hydra

  handleGameState :: ChessQueue -> Connection -> HeadId -> UTxOs -> Bool -> IO ()
  handleGameState queue cnx headId utxo isReplaying = do
    -- find output paying to game script address
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- getScriptAddress gameScriptFile network

    -- extract game state from inline datum encoded as Data with schema
    case extractGameState gameScriptAddress utxo of
      Left err ->
        -- NOTE: we ignore the error because it's possible it's caused
        -- by a change in the underlying contract or game data structure
        pure ()
      Right st ->
        processGameState st queue cnx headId utxo isReplaying
          >>= \e -> publish queue e

  processGameState st@ChessGame{game} queue cnx headId utxo isReplaying =
    let triggerEndGame winner = do
          unless isReplaying $ void $ async $ endGame queue cnx utxo
          pure $ GameEnded headId st winner
     in if game == Chess.initialGame
          then pure (GameStarted headId st [])
          else case Chess.checkState game of
            CheckMate White -> do
              -- FIXME this is wrong and a consequence of the incorrect structure of the
              -- application. The thread receiving messages should transform and transfer them
              -- as fast as possible but not do complicated tx handling
              triggerEndGame BWin
            Resigned White ->
              triggerEndGame BWin
            CheckMate Black ->
              triggerEndGame AWin
            Resigned Black ->
              triggerEndGame AWin
            _other ->
              pure $ GameChanged headId st []

  buildNewTx cnx args outFile sk = do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    protocolParametersFile <- findProtocolParametersFile network
    let fullArgs =
          [ "conway"
          , "transaction"
          , "build-raw"
          ]
            <> args
            <> [ "--required-signer"
               , sk
               , "--fee"
               , "0"
               , "--protocol-params-file"
               , protocolParametersFile
               , "--out-file"
               , outFile
               ]

    logWith logger $ BuildingTransaction outFile fullArgs

    callProcess cardanoCliExe fullArgs

  signTx cnx args outFile sk = do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    let signedFile = outFile <.> "signed"

    callProcess
      cardanoCliExe
      $ [ "conway"
        , "transaction"
        , "sign"
        , "--tx-file"
        , outFile
        , "--signing-key-file"
        , sk
        , "--out-file"
        , signedFile
        ]
        <> networkMagicArgs network
    pure signedFile

  submitNewTx cnx args sk = do
    txRawFile <- mkTempFile

    buildNewTx cnx args txRawFile sk

    signedFile <- signTx cnx args txRawFile sk

    envelope <- eitherDecode' @Value <$> LBS.readFile signedFile

    case envelope of
      Left err -> do
        -- FIXME: what to do here?
        logWith logger $ ErrorSubmittingTransaction err
        throwIO $ ServerException ("Error submitting transaction: " <> pack err)
      Right tx -> do
        WS.sendTextData cnx (encode $ NewTx tx)

        logWith logger $ SubmittedTransaction signedFile

  sendInit :: ChessQueue -> Connection -> [Text] -> IO HeadId
  sendInit queue cnx _unusedParties = do
    WS.sendTextData cnx (encode Init)
    timeout
      600_000_000
      ( waitFor queue $ \case
          HeadCreated headId _ -> Just headId
          _ -> Nothing
      )
      >>= maybe
        (throwIO $ ServerException "Timeout (10m) waiting for head Id")
        (\headId -> pure headId)

  sendCommit :: ChessQueue -> Connection -> Host -> Integer -> HeadId -> IO ()
  sendCommit queue cnx Host{host, port} _amount headId =
    try go >>= \case
      Left (err :: CommitError) -> logWith logger err
      Right{} -> pure ()
   where
    go = do
      logWith logger $ CommittingTo headId
      cardanoCliExe <- findCardanoCliExecutable
      socketPath <- findSocketPath network

      -- find collateral UTxO
      (gameSkFile, gameVkFile) <- findKeys Game network
      gameAddress <- getVerificationKeyAddress gameVkFile network

      collateralUTxO <-
        checkCollateralUTxO logger network gameAddress

      -- find game token UTxO
      utxo <-
        checkGameTokenIsAvailable logger network gameVkFile

      eloScriptFile <- findEloScriptFile network
      eloRedeemerFile <- findDatumFile "commit" () network
      txRawFile <- mkTempFile

      -- build script info
      let args =
            [ "--tx-in"
            , unpack (txIn collateralUTxO)
            , "--tx-in-collateral"
            , unpack (txIn collateralUTxO)
            , "--tx-in"
            , unpack (txIn utxo)
            , "--tx-in-script-file"
            , eloScriptFile
            , "--tx-in-inline-datum-present"
            , "--tx-in-redeemer-file"
            , eloRedeemerFile
            , "--tx-in-execution-units"
            , "(5000000000,5000000)" -- TODO: compute exact execution units, this is half the budget for preprod now
            ]

      buildNewTx cnx args txRawFile gameSkFile
      tx <- decodeFileStrict @Value txRawFile

      let commitBlueprint =
            object
              [ "blueprintTx" .= tx
              , "utxo" .= UTxOs [utxo, collateralUTxO]
              ]

      -- commit is now external, so we need to handle query to the server, signature and then
      -- submission via the cardano-cli
      request <- parseRequest ("POST http://" <> unpack host <> ":" <> show port <> "/commit")
      response <- httpLBS $ setRequestBodyJSON commitBlueprint request

      txFileRaw <-
        case statusCode (responseStatus response) of
          200 ->
            mkstemp "tx.raw" >>= \(fp, hdl) -> do
              LBS.hPutStr hdl $ responseBody response
              hClose hdl
              pure fp
          other ->
            throwIO $
              CommitError $
                "Commit transaction failed with error "
                  <> show other
                  <> " "
                  <> LT.unpack (LT.decodeUtf8 (responseBody response))
                  <> " for blueprint "
                  <> asString commitBlueprint

      callProcess
        cardanoCliExe
        $ [ "conway"
          , "transaction"
          , "sign"
          , "--tx-file"
          , txFileRaw
          , "--signing-key-file"
          , gameSkFile
          , "--out-file"
          , txFileRaw <.> "signed"
          ]
          <> networkMagicArgs network

      result <-
        readProcessWithExitCode
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
      logWith logger $ CardanoCliResult cardanoCliExe result

      case result of
        (ExitFailure{}, out, err) ->
          throwIO $ CommitError $ "Failed to submit transaction, out: " <> out <> ", err: " <> err
        _ -> pure ()

      logWith logger $ SubmittedTransaction (txFileRaw <.> "signed")

      timeout
        60_000_000
        ( waitFor queue $ \case
            FundCommitted _ party _ | party == me -> Just ()
            _ -> Nothing
        )
        >>= maybe
          (logWith logger $ WaitingForCommit me headId)
          (const $ logWith logger (CommittedTo headId))

  -- \| the new game transaction
  --
  -- It is made of:
  --
  -- \* each game token as input
  -- \* the current player's "deposit" as collateral
  -- \* a single game script output with the initial game
  -- \* the refunded deposit
  --
  -- The reason we need the deposit input is to provide an ADA-only collateral input
  -- which is required by the ledger rules
  newGame :: ChessQueue -> Connection -> IO ()
  newGame queue connection = do
    cardanoCliExe <- findCardanoCliExecutable
    (skFile, vkFile) <- findKeys Game network
    gameAddress <- getVerificationKeyAddress vkFile network
    socketPath <- findSocketPath network

    utxo <- collectUTxO queue connection

    let collateral = findCollateral gameAddress utxo
        collateralValue = lovelace $ value collateral
        pid = Token.validatorHashHex
        gameTokens = findGameTokens pid utxo
        balance = sum $ (\FullUTxO{value = Coins{lovelace}} -> lovelace) <$> gameTokens

    gameInputs <- concat <$> mapM makeGameInput gameTokens

    -- find chess game address
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- getScriptAddress gameScriptFile network

    let initGame =
          ChessGame
            { players = fmap (makePlayerState pid) gameTokens
            , game = Chess.initialGame
            }

    -- construct chess game inline datum
    gameDatumFile <- findDatumFile "game-state" initGame network

    protocolParametersFile <- findProtocolParametersFile network
    txRawFile <- mkTempFile

    let args =
          gameInputs
            <> ["--tx-in", unpack $ txIn collateral]
            <> ["--tx-in-collateral", unpack $ txIn collateral]
            <> [ "--tx-out"
               , gameScriptAddress
                  <> "+ "
                  <> show balance
                  <> " lovelace + "
                  <> List.intercalate " + " (makeValue pid <$> gameTokens)
               , "--tx-out-inline-datum-file"
               , gameDatumFile
               ]
            <> [ "--tx-out"
               , gameAddress <> "+ " <> show collateralValue <> " lovelace"
               ]

    submitNewTx connection args skFile

  playGame :: ChessQueue -> Connection -> GamePlay Chess -> IO ()
  playGame _ cnx (GamePlay End) = pure ()
  playGame queue cnx (GamePlay play@(ChessMove move)) =
    try go >>= \case
      Left (InvalidMove illegal) -> putStrLn $ "Invalid move:  " <> show illegal
      Left other -> putStrLn $ "Error looking for data:  " <> show other
      Right{} -> pure ()
   where
    go = do
      -- retrieve needed tools and keys
      cardanoCliExe <- findCardanoCliExecutable
      (skFile, vkFile) <- findKeys Game network
      gameAddress <- getVerificationKeyAddress vkFile network
      socketPath <- findSocketPath network

      -- find chess game script & address
      gameScriptFile <- findGameScriptFile network
      gameScriptAddress <- getScriptAddress gameScriptFile network

      -- retrieve current UTxO state
      utxo <- collectUTxO queue cnx

      -- find collateral to submit play tx
      let collateral = findCollateral gameAddress utxo

      -- find current game state
      gameState@ChessGame{game, players} <-
        case extractGameState gameScriptAddress utxo of
          Left err ->
            throwIO $ UTxOError $ "Cannot extract game state from: " <> err
          Right game -> pure game

      (gameStateTxIn, gameScriptValue) <- case extractGameTxIn gameScriptAddress utxo of
        Left err -> do
          throwIO $ UTxOError $ "Cannot extract game value: " <> err
        Right v -> pure v

      -- compute next game state
      nextGame <- either (throwIO . InvalidMove) pure $ Chess.apply move game

      let nextState = ChessGame{players, game = nextGame}

      -- construct chess game inline datum
      gameDatumFile <- findDatumFile "game-state" nextState network
      moveRedeemerFile <- findDatumFile "move" play network

      -- define transaction arguments
      let args =
            [ "--tx-in"
            , unpack $ txIn collateral
            , "--tx-in-collateral"
            , unpack $ txIn collateral
            , "--tx-in"
            , gameStateTxIn
            , "--tx-in-script-file"
            , gameScriptFile
            , "--tx-in-inline-datum-present"
            , "--tx-in-redeemer-file"
            , moveRedeemerFile
            , "--tx-in-execution-units"
            , "(500000000000,10000000000)"
            , "--tx-out"
            , gameScriptAddress <> " + " <> stringifyValue gameScriptValue
            , "--tx-out-inline-datum-file"
            , gameDatumFile
            , "--tx-out"
            , gameAddress <> "+ 8000000 lovelace" -- FIXME shoudl be value in collateral
            ]

      submitNewTx cnx args skFile

  endGame :: ChessQueue -> Connection -> UTxOs -> IO ()
  endGame queue cnx utxo =
    tryEndGame 3
   where
    tryEndGame :: Int -> IO ()
    tryEndGame 0 = pure ()
    tryEndGame n =
      try go >>= \case
        Left (NoSingleOwnGameToken own their) -> pure ()
        Left NoGameFound -> pure ()
        Left other -> putStrLn $ "Error building end game tx:  " <> show other
        Right{} -> do
          -- FIXME: The idea is that even if we apparently succeeded, the submitted txs can
          -- still fail asynchronoulsy in Hydra and we are kind of stuck, so let's try again
          -- after some delay until we get one or the other above exceptions.
          -- This is stupid just like quite a few things in this app but it is what it is for
          -- now
          threadDelay 1000000
          tryEndGame (n - 1)
    go = do
      -- retrieve needed tools and keys
      (skFile, vkFile) <- findKeys Game network
      gameVk <- deserialiseFromEnvelope @PublicKey vkFile
      let pkh = unpack $ pubKeyHashToHex $ pubKeyHash $ convert $ hash @_ @Blake2b_224 gameVk
      gameAddress <- getVerificationKeyAddress vkFile network

      -- find chess game script & address
      gameScriptFile <- findGameScriptFile network
      gameScriptAddress <- getScriptAddress gameScriptFile network

      -- find ELO script & address
      eloScriptFile <- findEloScriptFile network
      eloScriptAddress <- getScriptAddress eloScriptFile network

      -- find collateral to submit end game tx
      let collateral = findCollateral gameAddress utxo
          collateralValue = lovelace $ value collateral

      gameState <-
        case extractGameState gameScriptAddress utxo of
          Left err ->
            throwIO $ UTxOError $ "Cannot extract game state from: " <> err
          Right game -> pure game

      let
        ChessGame{players, game = Chess.Game{checkState}} = gameState

        gameResult = case checkState of
          CheckMate White -> BWin
          Resigned White -> BWin
          CheckMate Black -> AWin
          Resigned Black -> AWin
          _other -> Draw -- FIXME: there should be an explicit draw in the game state
        rawEloChange = eloGain (snd $ head players) (snd $ players !! 1) gameResult

        newElo =
          if pack pkh == pubKeyHashToHex (fst $ head players)
            then rawEloChange + snd (head players)
            else snd (players !! 1) - rawEloChange

      (gameStateTxIn, (adas, tokens)) <- case extractGameTxIn gameScriptAddress utxo of
        Left err -> do
          throwIO $ NoGameFound -- game probably closed?
        Right v -> pure v

      let (own, their) = List.partition (\(_, pk, _, _) -> pk == pkh) tokens

      logWith logger $ FoundGameTokens own their

      when (length own /= 1) $ throwIO $ NoSingleOwnGameToken own their

      args <-
        if null their
          then do
            -- construct chess game inline datum
            endGameRedeemerFile <- findDatumFile "endgame" End network

            -- define transaction arguments
            pure $
              [ "--tx-in"
              , unpack $ txIn collateral
              , "--tx-in-collateral"
              , unpack $ txIn collateral
              , "--tx-in"
              , gameStateTxIn
              , "--tx-in-script-file"
              , gameScriptFile
              , "--tx-in-inline-datum-present"
              , "--tx-in-redeemer-file"
              , endGameRedeemerFile
              , "--tx-in-execution-units"
              , "(100000000000,10000000000)"
              , "--tx-out"
              , eloScriptAddress <> " + " <> stringifyValue (adas, own)
              , "--tx-out-inline-datum-value"
              , show newElo
              , "--tx-out"
              , gameAddress <> "+ " <> show collateralValue <> " lovelace"
              ]
          else do
            -- construct chess game inline datum
            gameDatumFile <- findDatumFile "game-state" gameState network
            endGameRedeemerFile <- findDatumFile "endgame" End network

            -- define transaction arguments
            pure $
              [ "--tx-in"
              , unpack $ txIn collateral
              , "--tx-in-collateral"
              , unpack $ txIn collateral
              , "--tx-in"
              , gameStateTxIn
              , "--tx-in-script-file"
              , gameScriptFile
              , "--tx-in-inline-datum-present"
              , "--tx-in-redeemer-file"
              , endGameRedeemerFile
              , "--tx-in-execution-units"
              , "(100000000000,1000000000)"
              , "--tx-out"
              , gameScriptAddress <> " + " <> stringifyValue (adas - 2000000, their)
              , "--tx-out-inline-datum-file"
              , gameDatumFile
              , "--tx-out"
              , eloScriptAddress <> " + " <> stringifyValue (2000000, own)
              , "--tx-out-inline-datum-value"
              , show newElo
              , "--tx-out"
              , gameAddress <> "+ " <> show collateralValue <> " lovelace"
              ]

      submitNewTx cnx args skFile

  makeValue :: String -> FullUTxO -> String
  makeValue pid FullUTxO{value = Coins{natives}} =
    case Map.lookup (pack pid) natives of
      Just (Coin coins) -> case Map.toList coins of
        [(pkh, amount)] -> show amount <> " " <> pid <> "." <> unpack pkh
        _ -> error $ "Cannot find pkh from " <> show coins
      Nothing -> error $ "Cannot find " <> pid <> " in " <> show natives

  findCollateral :: String -> UTxOs -> FullUTxO
  findCollateral address (UTxOs utxo) =
    case filter (hasAddress address) utxo of
      (u : _) -> u
      _ -> error $ "Cannot find collateral for " <> address <> " from " <> asString utxo

  findGameTokens ::
    String ->
    UTxOs ->
    [FullUTxO]
  findGameTokens pid (UTxOs utxo) =
    filter (hasPolicyId pid) utxo

  hasPolicyId :: String -> FullUTxO -> Bool
  hasPolicyId pid FullUTxO{value = Coins{natives}} =
    isJust $ Map.lookup (pack pid) natives

  makeGameInput :: FullUTxO -> IO [String]
  makeGameInput FullUTxO{txIn} = do
    eloScriptFile <- findEloScriptFile network

    pure $
      [ "--tx-in"
      , unpack txIn
      , "--tx-in-script-file"
      , eloScriptFile
      , "--tx-in-inline-datum-present"
      , "--tx-in-redeemer-value"
      , "[]"
      , "--tx-in-execution-units"
      , "(1000000000,1000000000)"
      ]

  makePlayerState pid utxo@FullUTxO{value = Coins{natives}, inlineDatum} =
    let pkh = fromMaybe (error $ "Cannot find pkh from utxo " <> asString utxo) $ do
          Coin coins <- Map.lookup (pack pid) natives
          case Map.keys coins of
            [pkh] -> Just pkh
            _ -> Nothing
        elo = fromMaybe (error $ "No datum to extract Elo from " <> asString utxo) (inlineDatum >>= integerFromDatum)
     in (pubKeyHashFromHex pkh, elo)

  sendClose :: ChessQueue -> Connection -> IO ()
  sendClose queue cnx = do
    -- FIXME: should end game if it's not ended? Or prevent if game not ended?
    WS.sendTextData cnx (encode Close)
    timeout
      600_000_000
      ( waitFor queue $ \case
          HeadClosing headId -> Just ()
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (10m) waiting for head Id") (const $ WS.sendTextData cnx (encode Fanout))

  -- Collect only TxIn
  collectUTxO :: ChessQueue -> Connection -> IO UTxOs
  collectUTxO queue cnx = go 10
   where
    go :: Int -> IO UTxOs
    go 0 = throwIO $ ServerException "Timeout (10s) waiting for GetUTxO"
    go n = do
      WS.sendTextData cnx (encode GetUTxO)
      timeout
        10_000_000
        ( waitFor queue $ \case
            CollectUTxO (JsonContent txt) ->
              case fromJSON txt of
                Success utxo -> Just utxo
                Error err -> error err
            _ -> Nothing
        )
        >>= maybe (go (n - 1)) pure

data Request = Init | Close | Fanout | GetUTxO | NewTx Value
  deriving stock (Eq, Show, Generic)

instance ToJSON Request where
  toJSON = \case
    Init -> object ["tag" .= ("Init" :: Text)]
    Close -> object ["tag" .= ("Close" :: Text)]
    Fanout -> object ["tag" .= ("Fanout" :: Text)]
    GetUTxO -> object ["tag" .= ("GetUTxO" :: Text)]
    NewTx txEnvelope ->
      object
        [ "tag" .= ("NewTx" :: Text)
        , "transaction" .= txEnvelope
        ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj ->
    obj .: "tag" >>= \case
      ("Init" :: String) -> pure Init
      ("Close" :: String) -> pure Close
      ("Fanout" :: String) -> pure Close
      ("GetUTxO" :: String) -> pure GetUTxO
      ("NewTx" :: String) -> NewTx <$> obj .: "transaction"
      other -> fail $ "Unknown request type: " <> other

data Response = Response
  { output :: !Output
  , seq :: !Natural
  , timestamp :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)

data Output
  = HeadIsInitializing {headId :: HeadId, parties :: [HydraParty]}
  | Committed {headId :: HeadId, party :: HydraParty, utxo :: UTxOs}
  | HeadIsOpen {headId :: HeadId, utxo :: UTxOs}
  | Greetings {me :: HydraParty}
  | HeadIsAborted {headId :: HeadId, utxo :: UTxOs}
  | HeadIsFinalized {headId :: HeadId, utxo :: UTxOs}
  | HeadIsClosed {headId :: HeadId, snapshotNumber :: Int, contestationDeadline :: UTCTime}
  | ReadyToFanout {headId :: HeadId}
  | PostTxOnChainFailed {postChainTx :: Value, postTxError :: Value}
  | RolledBack
  | CommandFailed {clientInput :: Request}
  | GetUTxOResponse {headId :: HeadId, utxo :: UTxOs}
  | TxValid {headId :: HeadId, transaction :: Value}
  | TxInvalid {headId :: HeadId, utxo :: UTxOs, transaction :: Value, validationError :: Value}
  | SnapshotConfirmed {headId :: HeadId, snapshot :: Snapshot, signatures :: Value}
  | PeerConnected {peer :: Text}
  | PeerDisconnected {peer :: Text}
  | InvalidInput {reason :: Text, input :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Snapshot = Snapshot {headId :: HeadId, utxo :: UTxOs, confirmed :: [Value], number :: Word64}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "headId")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmed")
      <*> (obj .: "number")

instance FromJSON Response where
  parseJSON v = flip (withObject "Response") v $ \o -> do
    output <- parseJSON v
    seq <- o .: "seq"
    timestamp <- o .: "timestamp"
    pure $ Response{output, seq, timestamp}

-- -- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- -- the 'ClientEffect'.
-- data ServerOutput tx
--   = HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber}
--   | TxSeen {headId :: HeadId, transaction :: tx}
--   | TxExpired {headId :: HeadId, transaction :: tx}
--   | InvalidInput {reason :: String, input :: Text}
--   deriving (Generic)

withClient :: Logger -> Host -> (Connection -> IO a) -> IO a
withClient logger Host{host, port} action = do
  logWith logger $ ConnectingToHydra host port
  connectedOnce <- newIORef False
  tryConnect connectedOnce
 where
  tryConnect connectedOnce =
    doConnect connectedOnce `catch` \(e :: IOException) -> do
      readIORef connectedOnce >>= \case
        False -> do
          logWith logger $ WaitingForConnectionToHydra host port
          threadDelay 1_000_000
          tryConnect connectedOnce
        True -> throwIO e

  wsOptions = defaultConnectionOptions

  doConnect connectedOnce = WS.runClientWith (unpack host) port "/" wsOptions mempty $ \connection -> do
    atomicWriteIORef connectedOnce True
    logWith logger $ ConnectedToHydra host port
    res <- WS.withPingThread connection 10 (pure ()) $ action connection
    WS.sendClose connection ("Bye" :: Text)
    pure res
