{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Chess.GameState where

import Chess.Game (Game (..), Move)
import Chess.Plutus (pubKeyHashFromHex, pubKeyHashToHex)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusPrelude (first)
import PlutusTx qualified
import PlutusTx.Prelude (Integer)
import Test.QuickCheck (Arbitrary (..), frequency)
import Prelude (Bool (..), fmap, pure, ($), (<$>))
import Prelude qualified as Haskell

-- * Toplevel state/transition
data ChessGame = ChessGame
  { players :: [(PubKeyHash, Integer)]
  -- ^ Players with their Elo score identified by their pkh.
  -- Their might be one or 2 players at start and by convention the one whose
  -- pkh is closest to the starting game's hash value (??) is `White`, the other
  -- being `Black`.
  , game :: Game
  -- ^ State of the game.
  }
  deriving (Haskell.Eq, Haskell.Show, Generic)

PlutusTx.unstableMakeIsData ''ChessGame

instance ToJSON ChessGame where
  toJSON ChessGame{players, game} =
    object
      [ "game" .= game
      , "players" .= (first pubKeyHashToHex <$> players)
      ]

instance FromJSON ChessGame where
  parseJSON = withObject "ChessGame" $ \obj -> do
    game <- obj .: "game"
    players <- fmap (first pubKeyHashFromHex) <$> obj .: "players"
    pure ChessGame{game, players}

data ChessPlay
  = -- | A normal move.
    ChessMove Move
  | -- | End game.
    -- Only valid if the `game` has ended through a `Draw` or a `CheckMate`.
    -- If it's the case, then the player, identified by their pkh from the transaction's
    -- signatories, can collect their "reward".
    End
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ChessPlay

instance Arbitrary ChessPlay where
  arbitrary =
    frequency
      [ (1, pure End)
      , (9, ChessMove <$> arbitrary)
      ]

isMove :: ChessPlay -> Bool
isMove = \case
  ChessMove{} -> True
  End -> False
