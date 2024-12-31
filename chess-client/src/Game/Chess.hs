{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Chess where

import Chess.Elo.Score (Result)
import qualified Chess.Game as Chess
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Parse (parseMove)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (unpack)
import Game.Server (Game (..))
import Test.QuickCheck (Arbitrary (..))

type Chess = Chess.Game

instance Game Chess where
  type GameState Chess = ChessGame
  newtype GamePlay Chess = GamePlay {unMove :: ChessPlay}
  type GameEnd Chess = Result
  initialGame = const $ ChessGame{game = Chess.initialGame, players = []}
  readPlay =
    either (const Nothing) (Just . GamePlay . ChessMove) . parseMove . unpack

instance Show (GamePlay Chess) where
  show (GamePlay m) = show m

instance Eq (GamePlay Chess) where
  (GamePlay m) == (GamePlay m') = m == m'

instance ToJSON (GamePlay Chess) where
  toJSON (GamePlay move) = toJSON move

instance FromJSON (GamePlay Chess) where
  parseJSON = fmap GamePlay . parseJSON

instance Arbitrary (GamePlay Chess) where
  arbitrary = GamePlay <$> arbitrary
