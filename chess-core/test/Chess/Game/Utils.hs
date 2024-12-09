{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Chess.Game.Utils where

import Chess.Game

import Chess.Render (render)
import Data.Function ((&))
import Data.Text (unpack)
import Test.QuickCheck (
  Property,
  Testable,
  counterexample,
  property,
 )

-- * Generic Properties

isIllegal :: Game -> Move -> Property
isIllegal game move =
  isIllegalMoveMatching game move (== IllegalMove move)

isBlocked :: Game -> Move -> Property
isBlocked game move =
  isIllegalMoveMatching game move isBlocked'
    & counterexample ("game:\n" <> unpack (render game))
 where
  isBlocked' :: IllegalMove -> Bool
  isBlocked' = \case
    MoveBlocked{} -> True
    _ -> False

isIllegalMoveMatching :: (Testable a) => Game -> Move -> (IllegalMove -> a) -> Property
isIllegalMoveMatching game move predicate =
  case apply move game of
    Right game' ->
      property False
        & counterexample ("after:\n" <> unpack (render game'))
        & counterexample ("before:\n" <> unpack (render game))
        & counterexample ("move: " <> show move)
        & counterexample ("moves: " <> show (render <$> reverse (moves game')))
    Left err ->
      predicate err
        & counterexample ("error: " <> show err)
        & counterexample ("game:\n" <> unpack (render game))

isLegalMove ::
  (Testable a) =>
  Move ->
  Game ->
  (Game -> a) ->
  Property
isLegalMove move game predicate =
  case apply move game of
    Right game' ->
      predicate game'
        & counterexample ("game: \n" <> unpack (render game'))
    Left err ->
      property False
        & counterexample ("error: " <> show err)
        & counterexample ("game: \n" <> unpack (render game))
        & counterexample ("path: " <> show (path from to))
 where
  Move from to = move

pawnHasMoved :: Side -> Position -> Bool
pawnHasMoved side (Pos r _) = case side of
  White -> r > 1
  Black -> r < 6
