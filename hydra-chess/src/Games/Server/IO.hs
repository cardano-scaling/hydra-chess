{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Games.Server.IO where

import Chess.Game (Game (curSide), Move)
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Render (Render (..))
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import Game.Chess (Chess, unMove)
import Game.Server (Content (..), FromChain (..), IsChain)
import Games.Logging (Logger, logWith)
import System.Console.ANSI (hCursorBackward)
import System.IO (stdout)

notifyChessEvent :: (IsChain c) => Logger -> FromChain Chess c -> IO ()
notifyChessEvent logger = \case
  GameStarted{game = ChessGame{game}} -> do
    hCursorBackward stdout 1000
    putStrLn (unpack $ render game)
    putStrLn $ (show $ curSide game) <> " to play"
  GameEnded{game = ChessGame{game}, gameEnd} -> do
    hCursorBackward stdout 1000
    putStrLn (unpack $ render game)
    putStrLn (show gameEnd)
  GameChanged{game = ChessGame{game}, plays} -> do
    hCursorBackward stdout 1000
    putStrLn (unpack $ render game)
    putStrLn (unlines $ unpack . render <$> (catMaybes $ fmap selectMoves $ unMove <$> plays))
    putStrLn $ (show $ curSide game) <> " to play"
  OtherMessage JsonContent{} -> pure ()
  other -> logWith logger other

selectMoves :: ChessPlay -> Maybe Move
selectMoves = \case
  ChessMove move -> Just move
  End -> Nothing
