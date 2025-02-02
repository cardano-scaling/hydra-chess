{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Server where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text, intercalate, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, arbitraryPrintableChar, listOf, vectorOf)

newtype ServerException = ServerException {reason :: Text}
  deriving (Eq, Show)

instance Exception ServerException

class
  ( Show (Coin c)
  , Eq (Coin c)
  , Monoid (Coin c)
  , ToJSON (Coin c)
  , FromJSON (Coin c)
  , Show (Party c)
  , Eq (Party c)
  , ToJSON (Party c)
  , FromJSON (Party c)
  ) =>
  IsChain c
  where
  -- | Type of parties for the given chain.
  type Party c

  -- | Type of coins for the given chain.
  type Coin c

  -- | Retrieve a (unique for this chain) identifier for a given party.
  partyId :: Party c -> Text

  -- | Retrieve the amount in a coin.
  coinValue :: Coin c -> Integer

newtype HeadId = HeadId {headId :: Text}
  deriving newtype (Eq, Show, Ord, IsString, ToJSON, FromJSON)

instance Arbitrary HeadId where
  arbitrary = HeadId . decodeUtf8 . Hex.encode . BS.pack <$> vectorOf 16 arbitrary

class
  ( Eq (GamePlay g)
  , Show (GamePlay g)
  , ToJSON (GamePlay g)
  , FromJSON (GamePlay g)
  , Eq (GameEnd g)
  , Show (GameEnd g)
  , ToJSON (GameEnd g)
  , FromJSON (GameEnd g)
  , Eq (GameState g)
  , Show (GameState g)
  , ToJSON (GameState g)
  , FromJSON (GameState g)
  ) =>
  Game g
  where
  -- | The type of game state associated with g
  type GameState g :: Type

  -- | The type of game play associated with g
  data GamePlay g :: Type

  -- | The situation at end of game
  type GameEnd g :: Type

  -- | Starting game state.
  initialGame :: Int -> GameState g

  -- | Read play from `Text`
  readPlay :: Text -> Maybe (GamePlay g)

-- | A handle to some underlying server for a single Head.
data Server g c m = Server
  { initHead :: [Text] -> m HeadId
  -- ^ Initialises a head with given parties.
  -- Might throw an exception if something goes wrong before hitting the
  -- underlying chain.
  , play :: GamePlay g -> m ()
  -- ^ When the game is opened, do one play which is game dependent
  -- Might throw a `ServerException` if the play is invalid.
  , newGame :: m ()
  -- ^ When the game has ended, restarts a new one with initial state and possible
  -- plays.
  -- Might throw a `ServerException` if the play is invalid.
  , closeHead :: m ()
  -- ^ Close the given head, effectively stopping the game and committing back
  -- payoffs on-chain. Fanout will be posted automatically at end of contestation
  -- period.
  -- Might throw a `ServerException` if the play is invalid.
  , poll :: Integer -> Integer -> m (Indexed g c)
  -- ^ Poll server for latest `FromChain` messages available.
  -- Takes the first event to retrieve and the maximum number of elements to send back.
  -- It will return 0 or more messages, depending on what's available, and the index
  -- of the last known message.
  , getConfiguration :: m Text
  -- ^ Retrieve the current configuration of the server.
  }

data FromChain g c
  = HeadCreated {headId :: HeadId, parties :: [Party c]}
  | FundCommitted {headId :: HeadId, party :: Party c, coin :: Coin c}
  | HeadOpened {headId :: HeadId}
  | GameStarted {headId :: HeadId, game :: GameState g, plays :: [GamePlay g]}
  | GameChanged {headId :: HeadId, game :: GameState g, plays :: [GamePlay g]}
  | GameEnded {headId :: HeadId, game :: GameState g, gameEnd :: GameEnd g}
  | HeadClosing {headId :: HeadId}
  | HeadClosed {headId :: HeadId}
  | OtherMessage {content :: Content}
  | CollectUTxO {utxo :: Content} -- FIXME: this shows the whole abstraction leaks like the Titanic
  deriving (Generic)

deriving instance (Game g, IsChain c) => Show (FromChain g c)
deriving instance (Game g, IsChain c) => Eq (FromChain g c)
deriving instance (Game g, IsChain c) => ToJSON (FromChain g c)
deriving instance (Game g, IsChain c) => FromJSON (FromChain g c)

instance
  ( Arbitrary (Party c)
  , Arbitrary (Coin c)
  , Arbitrary (GameState g)
  , Arbitrary (GamePlay g)
  , Arbitrary (GameEnd g)
  ) =>
  Arbitrary (FromChain g c)
  where
  arbitrary = genericArbitrary uniform

data Indexed g c = Indexed {lastIndex :: Integer, events :: [FromChain g c]}
  deriving (Generic)

deriving instance (Game g, IsChain c) => Show (Indexed g c)
deriving instance (Game g, IsChain c) => Eq (Indexed g c)
deriving instance (Game g, IsChain c) => ToJSON (Indexed g c)
deriving instance (Game g, IsChain c) => FromJSON (Indexed g c)

instance
  ( Arbitrary (Party c)
  , Arbitrary (Coin c)
  , Arbitrary (GameState g)
  , Arbitrary (GamePlay g)
  , Arbitrary (GameEnd g)
  ) =>
  Arbitrary (Indexed g c)
  where
  arbitrary = genericArbitrary uniform

data Content = Content Text | JsonContent Value
  deriving stock (Show, Eq, Generic)

instance ToJSON Content where
  toJSON = \case
    Content text -> String text
    JsonContent value -> value

instance FromJSON Content where
  parseJSON = \case
    String s -> pure $ Content s
    other -> pure $ JsonContent other

instance Arbitrary Content where
  arbitrary = Content . pack <$> listOf arbitraryPrintableChar

data Host = Host {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary Host where
  arbitrary = Host <$> anyIp <*> anyPort
   where
    anyIp = intercalate "." . fmap (pack . show) <$> vectorOf 4 (arbitrary :: Gen Word8)
    anyPort = fromIntegral <$> (arbitrary :: Gen Word16)
