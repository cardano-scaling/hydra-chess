{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chess.Game.Castling where

import Chess.Game

import Chess.Game.Utils (isIllegalMoveMatching)
import Chess.Generators (
  RookLike (..),
  generateMove,
  possibleMovesFor,
 )
import Chess.Render (render)
import Control.Monad (foldM)
import Data.Function ((&))
import Data.Text (unpack)
import Test.Hspec (Expectation, SpecWith, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  property,
  suchThat,
  tabulate,
 )

castlingSpec :: SpecWith ()
castlingSpec = do
  prop "requires king and rook to be at initial position" prop_requires_king_rook_at_initial_position
  it "is possible kingside for White" white_can_castle_king_side
  it "is possible kingside for Black" black_can_castle_king_side
  it "is possible queen-side for White" white_can_castle_queen_side
  it "is possible queen-side for Black" black_can_castle_queen_side
  it "is a move that changes current side" changes_current_side
  it "is possible for both sides" both_can_castle_king_side
  prop "is not possible if king would move through position in check" prop_cannot_castle_if_king_would_be_in_check
  prop "is not possible if king already moved" prop_cannot_castle_if_king_has_moved
  prop "is not possible if there are pieces between king and rook" prop_cannot_castle_if_row_not_empty

prop_requires_king_rook_at_initial_position :: Side -> Castle -> Property
prop_requires_king_rook_at_initial_position side castle =
  forAllBlind (elements [King, Rook]) $ \castlingPiece ->
    forAll (possibleMovesWithout castlingPiece) $ \(m@(Move from _), g) ->
      not (isCastling m)
        & counterexample ("game:\n" <> unpack (render g))
        & tabulate "Side" [show side]
        & tabulate "Moved" [show $ piece <$> pieceAt from g]
 where
  possibleMovesWithout piece =
    let g = game{pieces = filter ((/= toRemove piece) . pos) $ pieces game}
     in (,g) <$> possibleMovesFor side g

  toCastlingPosition = foldM (flip apply) initialGame

  Right game = case (side, castle) of
    (White, KingCastle) -> toCastlingPosition whiteKingCastlingPosition
    (White, QueenCastle) -> toCastlingPosition whiteQueenCastlingPosition
    (Black, KingCastle) -> toCastlingPosition blackKingCastlingPosition
    (Black, QueenCastle) -> toCastlingPosition blackQueenCastlingPosition

  toRemove piece =
    case (piece, side, castle) of
      (King, White, _) -> Pos 0 4 -- Never happens in real game
      (King, Black, _) -> Pos 7 4 -- Never happens in real game
      (Rook, White, KingCastle) -> Pos 0 7
      (Rook, White, QueenCastle) -> Pos 0 0
      (Rook, Black, KingCastle) -> Pos 7 7
      (Rook, Black, QueenCastle) -> Pos 7 0
      other -> error ("should not happen: " <> show other)

prop_cannot_castle_if_row_not_empty :: Castle -> Property
prop_cannot_castle_if_row_not_empty castle =
  isIllegalMoveMatching initialGame (toMove castle) isErrPiecesBetween
 where
  isErrPiecesBetween = \case
    NoCastling PiecesInBetween{} -> True
    _other -> False

prop_cannot_castle_if_king_has_moved :: Side -> Castle -> Property
prop_cannot_castle_if_king_has_moved side castle =
  forAll nonCastlingMove $ \kingMove ->
    forAll (possibleMovesFor (flipSide side) game') $ \otherMove ->
      let Right game'' = foldM (flip apply) game' [kingMove, otherMove, revert kingMove]
       in forAll (possibleMovesFor (flipSide side) game'') $ \otherMove' ->
            case apply otherMove' game'' of
              Right g ->
                isIllegalMoveMatching g (toMove castle) (== NoCastling (PiecesMoved side))
                  & tabulate "Side" [show side]
                  & tabulate "Castle" [unpack $ render $ toMove castle]
              Left err -> property False & counterexample ("error : " <> show err)
 where
  nonCastlingMove =
    generateMove kingPosition game' `suchThat` (not . isCastling)
  moves = case (side, castle) of
    (White, KingCastle) -> whiteKingCastlingPosition
    (White, QueenCastle) -> whiteQueenCastlingPosition
    (Black, KingCastle) -> blackKingCastlingPosition
    (Black, QueenCastle) -> blackQueenCastlingPosition
  Right game' = foldM (flip apply) initialGame moves
  kingPosition = case side of
    White -> Pos 0 4
    Black -> Pos 7 4

toMove :: Castle -> Move
toMove = \case
  KingCastle -> CastleKing
  QueenCastle -> CastleQueen

white_can_castle_king_side :: Expectation
white_can_castle_king_side =
  let game = initialGame
      moves = whiteKingCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleKing =<< game' of
        Right g -> do
          findPieces King White g `shouldBe` [PieceOnBoard King White (Pos 0 6)]
          findPieces Rook White g `shouldBe` [PieceOnBoard Rook White (Pos 0 0), PieceOnBoard Rook White (Pos 0 5)]
        Left e -> fail ("cannot apply castling on king side: " <> show e)

white_can_castle_queen_side :: Expectation
white_can_castle_queen_side =
  let game = initialGame
      moves = whiteQueenCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleQueen =<< game' of
        Right g -> do
          findPieces King White g `shouldBe` [PieceOnBoard King White (Pos 0 2)]
          findPieces Rook White g `shouldBe` [PieceOnBoard Rook White (Pos 0 7), PieceOnBoard Rook White (Pos 0 3)]
        Left e -> fail ("cannot apply castling on queen side: " <> show e)

black_can_castle_king_side :: Expectation
black_can_castle_king_side =
  let game = initialGame
      moves = blackKingCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleKing =<< game' of
        Right g -> do
          findPieces King Black g `shouldBe` [PieceOnBoard King Black (Pos 7 6)]
          findPieces Rook Black g `shouldBe` [PieceOnBoard Rook Black (Pos 7 0), PieceOnBoard Rook Black (Pos 7 5)]
        Left e -> fail ("cannot apply castling for black on king side: " <> show e)

both_can_castle_king_side :: Expectation
both_can_castle_king_side =
  let game = initialGame
      moves = dualCastlingPosition
      game' = foldM (flip apply) game moves
   in case game' >>= apply CastleKing >>= apply CastleKing of
        Right g -> do
          findPieces King Black g `shouldBe` [PieceOnBoard King Black (Pos 7 6)]
          findPieces King White g `shouldBe` [PieceOnBoard King White (Pos 0 6)]
        Left e -> fail ("cannot apply castling for black then white on king side: " <> show e)

black_can_castle_queen_side :: Expectation
black_can_castle_queen_side =
  let game = initialGame
      moves = blackQueenCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleQueen =<< game' of
        Right g -> do
          findPieces King Black g `shouldBe` [PieceOnBoard King Black (Pos 7 2)]
          findPieces Rook Black g `shouldBe` [PieceOnBoard Rook Black (Pos 7 7), PieceOnBoard Rook Black (Pos 7 3)]
        Left e -> fail ("cannot apply castling for black on queen side: " <> show e <> "\n" <> unpack (render game'))

changes_current_side :: Expectation
changes_current_side =
  let game = initialGame
      moves = blackQueenCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleQueen =<< game' of
        Right Game{curSide} ->
          curSide `shouldBe` White
        Left e -> fail ("cannot apply castling for black on queen side: " <> show e <> "\n" <> unpack (render game'))

prop_cannot_castle_if_king_would_be_in_check :: Property
prop_cannot_castle_if_king_would_be_in_check =
  forAll arbitrary $ \side ->
    forAll (elements [CastleKing, CastleQueen]) $ \move ->
      forAll (elements (concatMap accessibleOrthogonally $ kingsPositions move side) `suchThat` notOnCastlingRow side) $ \threat ->
        forAll arbitrary $ \(RookLike piece) ->
          let game = mkGame side (PieceOnBoard piece (flipSide side) threat : castlingPosition side)
           in isIllegalMoveMatching game move (== NoCastling KingInCheck)
 where
  kingsPositions move side = case (move, side) of
    (CastleQueen, White) -> [Pos 0 4, Pos 0 2, Pos 0 3]
    (CastleQueen, Black) -> [Pos 7 4, Pos 7 2, Pos 7 3]
    (CastleKing, White) -> [Pos 0 4, Pos 0 5, Pos 0 6]
    (CastleKing, Black) -> [Pos 7 4, Pos 7 5, Pos 7 6]
    other -> error $ "unexpected move " <> show other

  notOnCastlingRow side p =
    case side of
      White -> row p /= 0
      Black -> row p /= 7
  castlingPosition side =
    case side of
      White ->
        [ PieceOnBoard King White (Pos 0 4)
        , PieceOnBoard Rook White (Pos 0 7)
        , PieceOnBoard Rook White (Pos 0 0)
        ]
      Black ->
        [ PieceOnBoard King Black (Pos 7 4)
        , PieceOnBoard Rook Black (Pos 7 7)
        , PieceOnBoard Rook Black (Pos 7 0)
        ]

whiteKingCastlingPosition :: [Move]
whiteKingCastlingPosition =
  [ Move (Pos 1 4) (Pos 3 4)
  , Move (Pos 6 4) (Pos 4 4)
  , Move (Pos 0 5) (Pos 1 4)
  , Move (Pos 6 5) (Pos 5 5)
  , Move (Pos 0 6) (Pos 2 5)
  , Move (Pos 7 5) (Pos 4 2)
  ]

whiteQueenCastlingPosition :: [Move]
whiteQueenCastlingPosition =
  [ Move (Pos 1 4) (Pos 3 4) -- e2-e4
  , Move (Pos 6 4) (Pos 4 4) -- e7-e5
  , Move (Pos 1 3) (Pos 2 3) -- d2-d3
  , Move (Pos 6 5) (Pos 4 5) -- f7-f5
  , Move (Pos 0 3) (Pos 1 4) -- d1-e2
  , Move (Pos 7 5) (Pos 4 2) -- f8-c5
  , Move (Pos 0 2) (Pos 3 5) -- c1-f4
  , Move (Pos 7 3) (Pos 6 4) -- d8-e7
  , Move (Pos 0 1) (Pos 2 2) -- b1-c3
  , Move (Pos 7 6) (Pos 5 5) -- g8-f6
  ]

blackKingCastlingPosition :: [Move]
blackKingCastlingPosition =
  [ Move (Pos 1 4) (Pos 3 4) -- e2-e4
  , Move (Pos 6 4) (Pos 4 4) -- e7-e5
  , Move (Pos 0 5) (Pos 1 4) -- f1-e2
  , Move (Pos 7 5) (Pos 4 2) -- f8-c5
  , Move (Pos 0 6) (Pos 2 5) -- g1-f3
  , Move (Pos 7 6) (Pos 5 5) -- g8-f6
  , Move (Pos 1 3) (Pos 2 3) -- d2-d3
  ]

dualCastlingPosition :: [Move]
dualCastlingPosition =
  [ Move (Pos 1 4) (Pos 3 4) -- e2-e4
  , Move (Pos 6 4) (Pos 4 4) -- e7-e5
  , Move (Pos 1 3) (Pos 3 3) -- d2-d4
  , Move (Pos 7 6) (Pos 5 5) -- g8-f6
  , Move (Pos 0 5) (Pos 2 3) -- f1-d3
  , Move (Pos 7 5) (Pos 5 3) -- f8-d6
  , Move (Pos 0 6) (Pos 2 5) -- g1-f3
  ]

blackQueenCastlingPosition :: [Move]
blackQueenCastlingPosition =
  [ Move (Pos 1 4) (Pos 3 4) -- e2-e4
  , Move (Pos 6 3) (Pos 4 3) -- d7-d5
  , Move (Pos 1 3) (Pos 2 3) -- d2-d3
  , Move (Pos 6 2) (Pos 5 2) -- c7-c6
  , Move (Pos 0 3) (Pos 1 4) -- d1-e2
  , Move (Pos 7 3) (Pos 5 3) -- d8-d6
  , Move (Pos 0 2) (Pos 2 4) -- c1-e3
  , Move (Pos 7 2) (Pos 3 6) -- c8-g4
  , Move (Pos 0 1) (Pos 1 3) -- b1-d2
  , Move (Pos 7 1) (Pos 6 3) -- b8-e7
  , Move (Pos 0 6) (Pos 2 5) -- g1-f3
  ]

data Castle = KingCastle | QueenCastle
  deriving (Eq, Show)

instance Arbitrary Castle where
  arbitrary = elements [KingCastle, QueenCastle]
