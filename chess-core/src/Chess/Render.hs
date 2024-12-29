{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Render where

import Chess.Game (
  Game (..),
  IllegalMove,
  Move (..),
  Piece (..),
  PieceOnBoard (PieceOnBoard),
  Position (..),
  Side (..),
  pieceAt,
 )
import Data.Char (chr, intToDigit, ord)
import Data.Text (Text, pack)
import Data.Text qualified as Text

class Render r where
  render :: r -> Text

instance Render Position where
  render (Pos r c) = pack [chr (ord 'a' + fromInteger c), chr (ord '1' + fromInteger r)]

instance Render Move where
  render = \case
    (Move f t) -> render f <> "-" <> render t
    CastleKing -> "O-O"
    CastleQueen -> "O-O-O"
    Quit -> "X"

instance Render Game where
  render game =
    let allPos = [piece r c | r <- [0 .. 7], c <- [0 .. 7]]
        piece r c =
          if odd (r + c)
            then white (character (pieceAt (Pos r c) game))
            else black (character (pieceAt (Pos r c) game))
        raws = reverse $ splitEvery 8 allPos
        rows = zipWith (\cs n -> intToDigit n : ' ' : concat cs) raws [8, 7 .. 1]
     in Text.unlines ((Text.pack <$> rows) <> ["  a b c d e f g h "])

instance Render IllegalMove where
  render move = pack $ "Illegal move: " <> show move

instance (Render a, Render b) => Render (Either a b) where
  render = either render render

black :: Char -> [Char]
black s = "\ESC[38;5;0m\ESC[48;2;199;132;67m" <> [s, ' '] <> "\ESC[0m"

white :: Char -> [Char]
white s = "\ESC[38;5;0m\ESC[48;2;240;194;148m" <> [s, ' '] <> "\ESC[0m"

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
 where
  (as, bs) = splitAt n xs

character :: Maybe PieceOnBoard -> Char
character = \case
  Just (PieceOnBoard Pawn White _) -> '♙'
  Just (PieceOnBoard Pawn Black _) -> '♟'
  Just (PieceOnBoard Rook White _) -> '♖'
  Just (PieceOnBoard Rook Black _) -> '♜'
  Just (PieceOnBoard Knight White _) -> '♘'
  Just (PieceOnBoard Knight Black _) -> '♞'
  Just (PieceOnBoard Bishop White _) -> '♗'
  Just (PieceOnBoard Bishop Black _) -> '♝'
  Just (PieceOnBoard Queen White _) -> '♕'
  Just (PieceOnBoard Queen Black _) -> '♛'
  Just (PieceOnBoard King White _) -> '♔'
  Just (PieceOnBoard King Black _) -> '♚'
  Nothing -> ' '
