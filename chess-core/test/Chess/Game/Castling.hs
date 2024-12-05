{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chess.Game.Castling where

import Chess.Game

import Chess.Game.Utils (isIllegal)
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
  property,
  suchThat,
  tabulate,
 )

castlingSpec :: SpecWith ()
castlingSpec = do
  it "is possible kingside for White" white_can_castle_king_side
  it "is possible kingside for Black" black_can_castle_king_side
  it "is possible queen-side for White" white_can_castle_queen_side
  it "is possible queen-side for Black" black_can_castle_queen_side
  prop "is not possible if king would move through position in check" prop_cannot_castle_if_king_would_be_in_check
  prop "is not possible if king already moved" prop_cannot_castle_if_king_has_moved

prop_cannot_castle_if_king_has_moved :: Side -> Castle -> Property
prop_cannot_castle_if_king_has_moved side castle =
  forAll (generateMove kingPosition game') $ \kingMove ->
    forAll (possibleMovesFor (flipSide side) game') $ \otherMove ->
      let Right game'' = foldM (flip apply) game' [kingMove, otherMove, revert kingMove]
       in forAll (possibleMovesFor (flipSide side) game'') $ \otherMove' ->
            case apply otherMove' game'' of
              Right g ->
                isIllegal g (toMove castle)
                  & tabulate "Side" [show side]
                  & tabulate "Caste" [unpack $ render $ toMove castle]
              Left err -> property False & counterexample ("error :  " <> show err)
 where
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

black_can_castle_queen_side :: Expectation
black_can_castle_queen_side =
  let game = initialGame
      moves = blackQueenCastlingPosition
      game' = foldM (flip apply) game moves
   in case apply CastleQueen =<< game' of
        Right g -> do
          findPieces King Black g `shouldBe` [PieceOnBoard King Black (Pos 7 2)]
          findPieces Rook Black g `shouldBe` [PieceOnBoard Rook Black (Pos 7 7), PieceOnBoard Rook Black (Pos 7 3)]
        Left e -> fail ("cannot apply castling for black on queen side: " <> show e)

prop_cannot_castle_if_king_would_be_in_check :: Property
prop_cannot_castle_if_king_would_be_in_check =
  forAll arbitrary $ \side ->
    forAll (elements [CastleKing, CastleQueen]) $ \move ->
      forAll (elements (concatMap accessibleOrthogonally $ kingsPositions move side) `suchThat` notOnCastlingRow side) $ \threat ->
        forAll arbitrary $ \(RookLike piece) ->
          let game = mkGame side (PieceOnBoard piece (flipSide side) threat : castlingPosition side)
           in isIllegal game move
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
