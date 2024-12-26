{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | JSON manipulation functions
module Games.Server.JSON (
  GameToken,
  FullUTxO (..),
  UTxOs (..),
  Coins (..),
  Coin (..),
  valueContains,
  GameTokens,
  asString,
  extractGameState,
  extractGameToken,
  extractGameTxIn,
  stringifyValue,
  hasAddress,
) where

import Chess.Data (fromJSONDatum)
import Chess.GameState (ChessGame (..))
import Data.Aeson (
  FromJSON,
  ToJSON (..),
  Value (..),
  encode,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Stack (HasCallStack)
import Game.Client.Console (Coin (..), Coins (..), natives, toListOfTokens)
import System.FilePath ((<.>))
import Prelude hiding (seq)

-- FIXME: this is lame it should not exist
type GameToken =
  ( String -- txin
  , String -- pkh
  , Integer -- lovelace
  , Integer -- elo
  )

type GameTokens = [GameToken]

newtype UTxOs = UTxOs [FullUTxO]
  deriving stock (Eq, Show)

data FullUTxO = FullUTxO
  { txIn :: Text
  , address :: Text
  , value :: Coins
  , datum :: Maybe ByteString
  , datumHash :: Maybe ByteString
  , inlineDatum :: Maybe Value
  , inlineDatumRaw :: Maybe ByteString
  , inlineDatumHash :: Maybe ByteString
  }
  deriving stock (Eq, Show)

instance ToJSON FullUTxO where
  toJSON utxo = object $ asPair utxo

asPair :: (Aeson.KeyValue e a) => FullUTxO -> [a]
asPair FullUTxO{txIn, address, value, datum, datumHash, inlineDatum, inlineDatumHash, inlineDatumRaw} =
  [ fromText txIn
      .= object
        [ "address" .= address
        , "value" .= value
        , "datumHash" .= fmap (decodeUtf8 . Hex.encode) datumHash
        , "inlineDatum" .= inlineDatum
        , "inlineDatumhash" .= fmap (decodeUtf8 . Hex.encode) inlineDatumHash
        , "inlineDatumRaw" .= fmap (decodeUtf8 . Hex.encode) inlineDatumRaw
        , "datum" .= fmap (decodeUtf8 . Hex.encode) datum
        ]
  ]

instance ToJSON UTxOs where
  toJSON (UTxOs utxos) = object $ concatMap asPair utxos

instance FromJSON UTxOs where
  parseJSON = withObject "UTxOs" $ \kv ->
    UTxOs <$> traverse parseUTxO (KeyMap.toList kv)
   where
    parseUTxO :: (Aeson.Key, Value) -> Aeson.Parser FullUTxO
    parseUTxO (k, v) =
      let txIn = Key.toText k
       in withObject "UTxO" (parseFullUTxO txIn) v

    parseFullUTxO :: Text -> Aeson.Object -> Aeson.Parser FullUTxO
    parseFullUTxO txIn obj = do
      address <- obj .: "address"
      value <- obj .: "value"
      datumHash <- parseHex <$> obj .:? "datumHash"
      inlineDatum <- obj .:? "inlineDatum"
      inlineDatumHash <- parseHex <$> obj .:? "inlineDatumhash"
      inlineDatumRaw <- parseHex <$> obj .:? "inlineDatumRaw"
      datum <- parseHex <$> obj .:? "datum"
      pure $ FullUTxO{txIn, address, value, datum, datumHash, inlineDatum, inlineDatumHash, inlineDatumRaw}

    parseHex :: Maybe Text -> Maybe ByteString
    parseHex = \case
      Just hex -> case Hex.decode $ encodeUtf8 hex of
        Right bs -> Just bs
        Left _ -> Nothing
      Nothing -> Nothing

valueContains :: Text -> FullUTxO -> Bool
valueContains token FullUTxO{value} =
  token `elem` toListOfTokens (natives value)

asString :: (ToJSON a) => a -> String
asString = unpack . decodeUtf8 . LBS.toStrict . encode

extractGameToken :: String -> String -> UTxOs -> Maybe FullUTxO
extractGameToken pid pkh (UTxOs utxos) = find (uTxOWithValue pid pkh) utxos

uTxOWithValue :: String -> String -> FullUTxO -> Bool
uTxOWithValue pid pkh FullUTxO{value} =
  foundCoins == Just 1
 where
  foundCoins = do
    Coin coins <- Map.lookup (pack pid) (natives value)
    Map.lookup (pack pkh) coins

hasAddress :: String -> FullUTxO -> Bool
hasAddress addr FullUTxO{address} = address == pack addr

extractGameState :: (HasCallStack) => String -> UTxOs -> Either Text ChessGame
extractGameState address (UTxOs utxo) =
  case List.find (hasAddress address) utxo of
    Nothing -> Left $ "No output at address " <> pack address <> " for utxo"
    Just u -> findGameState u

extractGameTxIn :: String -> UTxOs -> Either Text (String, (Integer, GameTokens))
extractGameTxIn address (UTxOs utxo) =
  case List.find (hasAddress address) utxo of
    Nothing -> Left $ "No output at address " <> pack address <> " for utxo"
    Just FullUTxO{txIn, value} -> (unpack txIn,) <$> extractValue value

extractValue :: Coins -> Either Text (Integer, GameTokens)
extractValue Coins{lovelace, natives} =
  Right $ (lovelace,) $ Map.foldMapWithKey extractToken natives

extractToken :: Text -> Coin -> GameTokens
extractToken pid (Coin coins) =
  map (\(pkh, val) -> (unpack pid, unpack pkh, val, 0)) $ Map.toList coins

stringifyValue :: (Integer, GameTokens) -> String
stringifyValue (adas, tokens) =
  show adas <> " lovelace + " <> List.intercalate " + " (map stringify tokens)
 where
  stringify (pid, tok, val, _) = show val <> " " <> pid <.> tok

findGameState :: FullUTxO -> Either Text ChessGame
findGameState utxo@FullUTxO{inlineDatum} =
  maybe (error $ "Cannot find game state in empty datum " <> asString utxo) fromJSONDatum inlineDatum
