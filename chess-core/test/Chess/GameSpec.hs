{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chess.GameSpec where

import Chess.Game

import Chess.Generators (
  BishopLike (..),
  RookLike (..),
  anyColumn,
  anyPawn,
  anyPos,
  anyRow,
  anyValidPawn,
  generateMove,
 )
import Chess.Render (render)
import Control.Monad (foldM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (unpack)
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  Testable,
  choose,
  conjoin,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  property,
  suchThat,
  tabulate,
  withMaxSuccess,
  (===),
  (==>),
 )

spec :: Spec
spec = parallel $ do
  describe "Generators" $ do
    prop "generates 2 moves at start for pawns" prop_generate_2_starting_moves_for_pawns
  describe "Path" $ do
    prop "compute path of same length from both ends" prop_generate_paths_from_both_ends
    prop "compute diagonal paths" prop_compute_diagonal_paths
  describe "Pawn" $ do
    prop "can move a white pawn one or 2 squares at start of game" prop_can_move_pawn_one_or_2_squares_at_start
    prop "can move a white pawn one square after start of game" prop_can_move_pawn_one_square_after_start
    prop "cannot move a white pawn more than 2 squares at start of game" prop_cannot_move_a_pawn_more_than_2_squares
    prop "cannot move a white pawn more than 1 square after it moved" prop_cannot_move_a_pawn_more_than_1_square_after_it_moved
    prop "cannot move a white pawn if there's another piece at destination" prop_cannot_move_a_pawn_where_there_is_a_piece
    prop "white pawn can take piece when moving diagonally" prop_pawn_takes_piece_diagonally
    prop "white pawn cannot move diagonally" prop_pawn_cannot_move_diagonally
    prop "white pawn cannot move backwards" prop_pawn_cannot_move_backwards
    prop "can move a black pawn one or 2 squares at start of game" prop_can_move_black_pawn_one_or_2_squares_at_start
    prop "can move a pawn in its column only" prop_can_move_black_pawn_in_its_column_only
  describe "Rook & Queen" $ do
    prop "can move horizontally or vertically any number of squares" prop_can_move_rook_horizontally
    prop "can move vertically any number of squares" prop_can_move_rook_vertically
    prop "can take enemy piece at moved location" prop_can_take_enemy_piece
    prop "cannot move if blocked by other piece" prop_rook_cannot_move_if_blocked
  describe "Rook only" $ do
    prop "cannot take enemy piece if move is illegal" prop_cannot_take_enemy_piece_moving_illegally
  describe "Bishop & Queen" $ do
    prop "can move diagonally any number of squares" prop_bishop_can_move_diagonally
    prop "can take enemy piece at moved location" prop_bishop_can_take_enemy_piece
    prop "cannot move if blocked by other piece" prop_bishop_cannot_move_if_blocked
  describe "Bishop only" $ do
    prop "cannot move orthogonally" prop_bishop_cannot_move_orthogonally
  describe "Knight" $ do
    prop "can move 2 squares in 1 one direction, 1 square in the other" prop_knight_can_move
    prop "can take piece at destination" prop_knight_can_take_piece
  describe "King" $ do
    prop "can move 1 square in all directions" prop_king_moves_one_square
    prop "can take adjacent piece" prop_king_takes_adjacent_piece
  describe "Castling" $ do
    it "is possible kingside for White" white_can_castle_king_side
    it "is possible kingside for Black" black_can_castle_king_side
    it "is possible queen-side for White" white_can_castle_queen_side
    it "is possible queen-side for Black" black_can_castle_queen_side
    prop "is not possible if king would move through position in check" prop_cannot_castle_if_king_would_be_in_check
  describe "Check" $ do
    prop "is set if next move can take King" prop_is_check_if_next_move_can_take_king
    it "is set if move uncover a piece that can take King" is_check_if_move_uncovers_attacking_piece
    prop "move not removing check is illegal when is set" prop_move_must_remove_check
  describe "CheckMate" $ do
    prop "is check mate given king cannot evade check" is_check_mate_given_cannot_evade_check
    prop "no move is possible after check mate" no_move_possible_after_check_mate
  describe "Side" $ do
    prop "cannot play same side twice in a row" prop_cannot_play_same_side_twice_in_a_row
  describe "General" $ do
    prop "cannot pass (move to the same position)" prop_cannot_pass
    prop "cannot move from empty position" prop_cannot_move_empty_position

is_check_mate_given_cannot_evade_check :: Property
is_check_mate_given_cannot_evade_check = do
  let game =
        Game
          Black
          NoCheck
          [ PieceOnBoard King White (Pos 0 0)
          , PieceOnBoard Pawn White (Pos 1 0)
          , PieceOnBoard Knight Black (Pos 3 2)
          , PieceOnBoard Rook Black (Pos 4 4)
          ]
  isLegalMove (Move (Pos 4 4) (Pos 0 4)) game (isCheckMate White)

no_move_possible_after_check_mate :: Property
no_move_possible_after_check_mate =
  let game =
        Game
          Black
          NoCheck
          [ PieceOnBoard King White (Pos 0 0)
          , PieceOnBoard Pawn White (Pos 1 0)
          , PieceOnBoard Knight Black (Pos 3 2)
          , PieceOnBoard Rook Black (Pos 4 4)
          ]
      Right game'@Game{pieces} = apply (Move (Pos 4 4) (Pos 0 4)) game
      movesForWhite = foldMap ((`possibleMoves` game') . pos) (filter ((== White) . side) pieces)
   in null movesForWhite
        & counterexample ("game':\n" <> unpack (render game'))

prop_move_must_remove_check :: Side -> Property
prop_move_must_remove_check side =
  forAllBlind anyPos $ \king ->
    forAllBlind (elements $ accessibleOrthogonally king) $ \check ->
      forAllBlind (anyPos `suchThat` \p -> (p /= king) && (p /= check)) $ \other ->
        forAll (arbitrary `suchThat` (/= King)) $ \otherPiece ->
          forAll arbitrary $ \(RookLike piece) ->
            let game =
                  Game
                    side
                    (Check side)
                    [ PieceOnBoard King side king
                    , PieceOnBoard piece (flipSide side) check
                    , PieceOnBoard otherPiece side other
                    ]
                moves = possibleMoves other game
             in not (null moves) ==> forAll (elements moves) $ \move ->
                  isLegalMove move game (not . isCheck side)
                    & counterexample ("king: " <> show king)
                    & counterexample ("other: " <> show other)
                    & counterexample ("attacker: " <> show check)

is_check_if_move_uncovers_attacking_piece :: Expectation
is_check_if_move_uncovers_attacking_piece = do
  let king = PieceOnBoard King Black (Pos 3 7)
      bishop = PieceOnBoard Bishop White (Pos 0 4)
      pawn = PieceOnBoard Pawn White (Pos 1 5)
      game = mkGame White [king, bishop, pawn]
      move = Move (Pos 1 5) (Pos 2 5)
  apply move game `shouldSatisfy` \case
    Right game' -> isCheck Black game'
    Left{} -> False

prop_is_check_if_next_move_can_take_king :: Side -> Property
prop_is_check_if_next_move_can_take_king side =
  forAll anyPos $ \king ->
    forAll (elements $ accessibleOrthogonally king) $ \check ->
      forAll (elements (accessibleOrthogonally check) `suchThat` (notOrthogonalTo king)) $ \from ->
        forAll arbitrary $ \(RookLike piece) ->
          let game = mkGame (flipSide side) [PieceOnBoard King side king, PieceOnBoard piece (flipSide side) from]
           in isLegalMove (Move from check) game (isCheck side)

prop_king_moves_one_square :: Side -> Property
prop_king_moves_one_square side =
  forAll anyPos $ \from ->
    forAll (elements $ adjacentTo from) $ \to ->
      let game = mkGame side [PieceOnBoard King side from]
       in isLegalMove (Move from to) game (== mkGame (flipSide side) [PieceOnBoard King side to])

prop_king_takes_adjacent_piece :: Side -> Property
prop_king_takes_adjacent_piece side =
  forAll anyPos $ \from ->
    forAll (elements $ adjacentTo from) $ \to ->
      forAll arbitrary $ \piece ->
        let game = mkGame side [PieceOnBoard King side from, PieceOnBoard piece (flipSide side) to]
         in isLegalMove (Move from to) game (== mkGame (flipSide side) [PieceOnBoard King side to])

prop_knight_can_move :: Side -> Property
prop_knight_can_move side =
  forAll anyPos $ \from ->
    forAll (elements $ accessibleByKnight from) $ \to ->
      let game =
            mkGame
              side
              [PieceOnBoard Knight side from]
       in isLegalMove (Move from to) game (== mkGame (flipSide side) [PieceOnBoard Knight side to])

prop_knight_can_take_piece :: Side -> Property
prop_knight_can_take_piece side =
  forAll anyPos $ \from ->
    forAll (elements $ accessibleByKnight from) $ \to ->
      forAll arbitrary $ \piece ->
        let game =
              mkGame
                side
                [PieceOnBoard Knight side from, PieceOnBoard piece (flipSide side) to]
         in isLegalMove (Move from to) game (== mkGame (flipSide side) [PieceOnBoard Knight side to])

prop_compute_diagonal_paths :: Property
prop_compute_diagonal_paths =
  forAll anyPos $ \pos ->
    forAll (elements $ accessibleDiagonals pos) $ \diagonal ->
      let thePath = path pos diagonal
       in length (positions thePath) == fromInteger (abs (row diagonal - row pos))
            & counterexample ("path: " <> show thePath)

prop_bishop_cannot_move_if_blocked :: BishopLike -> Property
prop_bishop_cannot_move_if_blocked (BishopLike piece) =
  forAll arbitrary $ \side ->
    forAll threePiecesDiagonallyAligned $ \(pos, pos1, pos2) ->
      let game =
            mkGame
              side
              [ PieceOnBoard piece side pos
              , PieceOnBoard Pawn side pos1
              , PieceOnBoard Pawn (flipSide side) pos2
              ]
       in isBlocked game (Move pos pos2)
 where
  threePiecesDiagonallyAligned =
    elements
      [ (Pos i i, Pos (i + dx) (i + dy), Pos (i + 2 * dx) (i + 2 * dy))
      | i <- [2 .. 5]
      , dx <- [-1, 1]
      , dy <- [-1, 1]
      ]

prop_bishop_can_move_diagonally :: BishopLike -> Side -> Property
prop_bishop_can_move_diagonally (BishopLike piece) side =
  forAll anyPos $ \from ->
    forAll (elements $ accessibleDiagonals from) $ \to ->
      let game =
            mkGame
              side
              [PieceOnBoard piece side from]
       in isLegalMove (Move from to) game (== mkGame (flipSide side) [PieceOnBoard piece side to])

prop_bishop_cannot_move_orthogonally :: Side -> Property
prop_bishop_cannot_move_orthogonally side =
  forAll anyPos $ \from ->
    forAll (elements $ accessibleOrthogonally from) $ \to ->
      let game =
            mkGame
              side
              [PieceOnBoard Bishop side from]
       in isIllegal game (Move from to)

prop_bishop_can_take_enemy_piece :: BishopLike -> Property
prop_bishop_can_take_enemy_piece (BishopLike piece) =
  forAll anyPos $ \pos ->
    forAll arbitrary $ \side ->
      let startGame = mkGame side [PieceOnBoard piece side pos]
       in forAll (elements $ possibleMoves pos startGame) $ \move@(Move _ to) ->
            let game =
                  mkGame
                    side
                    [ PieceOnBoard piece side pos
                    , PieceOnBoard Pawn (flipSide side) to
                    ]
             in isLegalMove move game (\game' -> length (findPieces Pawn (flipSide side) game') == 0)

prop_generate_paths_from_both_ends :: Property
prop_generate_paths_from_both_ends =
  forAll pairOfPositionsOnPath $ \(pos1, pos2) ->
    let path1 = path pos1 pos2
        path2 = path pos2 pos1
     in length (positions path1) == length (positions path2)
          & counterexample ("path1: " <> show path1)
          & counterexample ("path2: " <> show path2)
 where
  pairOfPositionsOnPath = do
    base@(Pos r c) <- anyPos
    elements $
      [(base, Pos r' c) | r' <- [0 .. 7], r' /= r]
        <> [(base, Pos r c') | c' <- [0 .. 7], c' /= c]

prop_rook_cannot_move_if_blocked :: Property
prop_rook_cannot_move_if_blocked =
  forAll arbitrary $ \side ->
    forAll threePiecesAligned $ \(pos, pos1, pos2) ->
      let game =
            mkGame
              side
              [ PieceOnBoard Rook side pos
              , PieceOnBoard Pawn side pos1
              , PieceOnBoard Rook (flipSide side) pos2
              ]
       in conjoin
            [ isBlocked game (Move pos pos2)
            , isBlocked (game{curSide = flipSide side}) (Move pos2 pos)
            ]
 where
  threePiecesAligned =
    elements
      [ (Pos r c, Pos r' c, Pos r'' c)
      | c <- [0 .. 7]
      , r <- [0 .. 5]
      , r' <- [r + 1 .. 7]
      , r'' <- [r' + 1 .. 7]
      ]

prop_cannot_take_enemy_piece_moving_illegally :: Property
prop_cannot_take_enemy_piece_moving_illegally =
  forAll anyPos $ \pos ->
    forAll arbitrary $ \side ->
      forAll (illegalMoves pos) $ \move@(Move _ to) ->
        let game =
              mkGame
                side
                [ PieceOnBoard Rook side pos
                , PieceOnBoard Pawn (flipSide side) to
                ]
         in isIllegal game move
 where
  illegalMoves from@(Pos r c) = Move from <$> anyPos `suchThat` \(Pos r' c') -> r' /= r && c' /= c

prop_can_take_enemy_piece :: RookLike -> Property
prop_can_take_enemy_piece (RookLike piece) =
  forAll anyPos $ \pos ->
    forAll arbitrary $ \side ->
      let startGame = mkGame side [PieceOnBoard piece side pos]
       in forAll (elements $ possibleMoves pos startGame) $ \move@(Move _ to) ->
            let game =
                  mkGame
                    side
                    [ PieceOnBoard piece side pos
                    , PieceOnBoard Pawn (flipSide side) to
                    ]
             in isLegalMove move game (\game' -> length (findPieces Pawn (flipSide side) game') == 0)

prop_cannot_pass :: Property
prop_cannot_pass =
  forAll arbitrary $ \game@Game{curSide, pieces} ->
    let moveInPlace = filter ((/= curSide) . side) pieces <&> \(PieceOnBoard _ _ pos) -> Move pos pos
     in conjoin (notMoving game <$> moveInPlace)
          & tabulate "Piece" (show . piece <$> pieces)
          & withMaxSuccess 10
 where
  notMoving :: Game -> Move -> Property
  notMoving game move =
    apply move game === Left (NotMoving move)

prop_can_move_rook_horizontally :: RookLike -> Property
prop_can_move_rook_horizontally (RookLike piece) =
  forAll anyPos $ \pos@(Pos r c) ->
    forAll arbitrary $ \side ->
      forAll (anyColumn `suchThat` (/= c)) $ \col ->
        let newPos = (Pos r col)
            game = mkGame side [PieceOnBoard piece side pos]
            move = Move pos newPos
         in isLegalMove
              move
              game
              (== mkGame (flipSide side) [PieceOnBoard piece side newPos])

prop_can_move_rook_vertically :: RookLike -> Property
prop_can_move_rook_vertically (RookLike piece) =
  forAll anyPos $ \pos@(Pos r c) ->
    forAll arbitrary $ \side ->
      forAll (anyRow `suchThat` (/= r)) $ \row ->
        let newPos = (Pos row c)
            game = mkGame side [PieceOnBoard piece side pos]
            move = Move pos newPos
         in isLegalMove
              move
              game
              (== mkGame (flipSide side) [PieceOnBoard piece side newPos])

prop_pawn_cannot_move_backwards :: Side -> Property
prop_pawn_cannot_move_backwards side =
  forAll anyPos $ \pos@(Pos r c) ->
    let game = mkGame side [PieceOnBoard Pawn side pos]
        offset = case side of
          White -> -1
          Black -> 1
        move = Move pos (Pos (r + offset) c)
     in isIllegal game move

prop_generate_2_starting_moves_for_pawns :: Side -> Property
prop_generate_2_starting_moves_for_pawns curSide =
  let game = initialGame{curSide}
   in forAll (anyPawn curSide game) $ \pos ->
        let moves = possibleMoves pos game
         in length moves == 2
              & counterexample ("possible moves: " <> show moves)

prop_can_move_black_pawn_in_its_column_only :: Property
prop_can_move_black_pawn_in_its_column_only =
  forAll (anyPawn Black initialGame) $ \from@(Pos row col) ->
    forAll
      ( elements [0 .. 7]
          `suchThat` \c' -> c' >= col + 2 || c' <= col - 2
      )
      $ \col' ->
        let move = Move from (Pos (row - 1) col')
         in isIllegal (initialGame{curSide = Black}) move

prop_cannot_play_same_side_twice_in_a_row :: Side -> Property
prop_cannot_play_same_side_twice_in_a_row side =
  forAll (anyPawn side initialGame) $ \pos ->
    let game = initialGame{curSide = side}
     in forAll (generateMove pos game) $ \move@(Move _ to@(Pos c r)) ->
          let game' = case apply move game of
                Right g -> g
                Left err -> error $ "unexpected invalid move " <> show err
              bit = case side of
                White -> 1
                Black -> -1
              move' = Move to (Pos c (r + bit))
           in apply move' game'
                === Left (WrongSideToPlay (flipSide side) move')
                & counterexample ("before: " <> show game')

prop_can_move_pawn_one_square_after_start :: Side -> Property
prop_can_move_pawn_one_square_after_start side =
  forAll (anyPos `suchThat` pawnHasMoved side) $ \pos@(Pos row col) ->
    let offset = case side of
          White -> 1
          Black -> -1
        game = mkGame side [PieceOnBoard Pawn side pos]
        move = Move (Pos row col) (Pos (row + offset) col)
     in isLegalMove move game (/= initialGame)

prop_pawn_takes_piece_diagonally :: Property
prop_pawn_takes_piece_diagonally =
  forAll (anyValidPawn White) $ \pos@(Pos r c) ->
    forAll (elements [-1, 1]) $ \diagonal ->
      let targetPos = Pos (r + 1) (c + diagonal)
       in forAll (anyPos `suchThat` \p -> p /= pos && p /= targetPos) $ \otherPos ->
            let game =
                  mkGame
                    White
                    [ PieceOnBoard Pawn White pos
                    , PieceOnBoard Pawn Black targetPos
                    , PieceOnBoard Pawn Black otherPos
                    ]
                move = Move pos targetPos
             in case apply move game of
                  Right game' ->
                    length (findPieces Pawn Black game')
                      === 1
                      & counterexample ("end game: " <> show game')
                      & counterexample ("move: " <> show move)
                      & counterexample ("start game: " <> show game)
                  Left err ->
                    property False
                      & counterexample ("game: " <> show err)

prop_pawn_cannot_move_diagonally :: Property
prop_pawn_cannot_move_diagonally =
  forAll (anyValidPawn White) $ \pos@(Pos r c) ->
    forAll (elements [-1, 1]) $ \diagonal ->
      let targetPos = Pos (r + 1) (c + diagonal)
       in forAll (anyPos `suchThat` \p -> p /= pos && p /= targetPos) $ \otherPos ->
            let game =
                  mkGame
                    White
                    [ PieceOnBoard Pawn White pos
                    , PieceOnBoard Pawn Black otherPos
                    ]
                move = Move pos targetPos
             in isIllegal game move

prop_cannot_move_a_pawn_where_there_is_a_piece :: Property
prop_cannot_move_a_pawn_where_there_is_a_piece =
  forAll (anyPawn White initialGame) $ \pos@Pos{row, col} ->
    forAll (choose (1, 2)) $ \offset ->
      let game = mkGame White [PieceOnBoard Pawn White pos, PieceOnBoard Pawn White $ Pos (row + 1) col]
          move = Move pos (Pos (row + offset) col)
       in isIllegal game move

prop_can_move_pawn_one_or_2_squares_at_start :: Property
prop_can_move_pawn_one_or_2_squares_at_start =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let result = apply (Move (Pos row col) (Pos (row + offset) col)) initialGame
       in case result of
            Right game' ->
              game' /= initialGame && length (findPieces Pawn White game') == 8
                & counterexample ("game: " <> show game')
            Left err ->
              property False
                & counterexample ("error: " <> show err)

prop_can_move_black_pawn_one_or_2_squares_at_start :: Property
prop_can_move_black_pawn_one_or_2_squares_at_start =
  forAll (anyPawn Black game) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let move = Move (Pos row col) (Pos (row - offset) col)
          result = apply move game
       in case result of
            Right game' ->
              game' /= game && length (findPieces Pawn Black game') == 8
                & counterexample ("end game: " <> show game')
                & counterexample ("move: " <> show move)
            Left err ->
              property False
                & counterexample ("error: " <> show err)
 where
  game = initialGame{curSide = Black}

prop_cannot_move_a_pawn_more_than_2_squares :: Property
prop_cannot_move_a_pawn_more_than_2_squares =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (3, 6)) $ \offset ->
      let move = Move (Pos row col) (Pos (row + offset) col)
       in isIllegal initialGame move

prop_cannot_move_a_pawn_more_than_1_square_after_it_moved :: Side -> Property
prop_cannot_move_a_pawn_more_than_1_square_after_it_moved side =
  forAll (anyPos `suchThat` pawnHasMoved side) $ \pos@(Pos row col) ->
    let game = mkGame side [PieceOnBoard Pawn side pos]
        move = Move (Pos row col) (Pos (row + 2) col)
     in isIllegal game move

prop_cannot_move_empty_position :: Property
prop_cannot_move_empty_position =
  forAll anyPos $ \from ->
    forAll (anyPos `suchThat` (/= from)) $ \to ->
      let game = mkGame White []
          move = Move from to
       in apply move game === Left (NoPieceToMove from)

white_can_castle_king_side :: Expectation
white_can_castle_king_side =
  let game = initialGame
      moves =
        [ Move (Pos 1 4) (Pos 3 4)
        , Move (Pos 6 4) (Pos 4 4)
        , Move (Pos 0 5) (Pos 1 4)
        , Move (Pos 6 5) (Pos 5 5)
        , Move (Pos 0 6) (Pos 2 5)
        , Move (Pos 7 5) (Pos 4 2)
        ]
      game' = foldM (flip apply) game moves
   in case apply CastleKing =<< game' of
        Right g -> do
          findPieces King White g `shouldBe` [PieceOnBoard King White (Pos 0 6)]
          findPieces Rook White g `shouldBe` [PieceOnBoard Rook White (Pos 0 0), PieceOnBoard Rook White (Pos 0 5)]
        Left e -> fail ("cannot apply castling on king side: " <> show e)

white_can_castle_queen_side :: Expectation
white_can_castle_queen_side =
  let game = initialGame
      moves =
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
      game' = foldM (flip apply) game moves
   in case apply CastleQueen =<< game' of
        Right g -> do
          findPieces King White g `shouldBe` [PieceOnBoard King White (Pos 0 2)]
          findPieces Rook White g `shouldBe` [PieceOnBoard Rook White (Pos 0 7), PieceOnBoard Rook White (Pos 0 3)]
        Left e -> fail ("cannot apply castling on queen side: " <> show e)

black_can_castle_king_side :: Expectation
black_can_castle_king_side =
  let game = initialGame
      moves =
        [ Move (Pos 1 4) (Pos 3 4) -- e2-e4
        , Move (Pos 6 4) (Pos 4 4) -- e7-e5
        , Move (Pos 0 5) (Pos 1 4) -- f1-e2
        , Move (Pos 7 5) (Pos 4 2) -- f8-c5
        , Move (Pos 0 6) (Pos 2 5) -- g1-f3
        , Move (Pos 7 6) (Pos 5 5) -- g8-f6
        , Move (Pos 1 3) (Pos 2 3) -- d2-d3
        ]
      game' = foldM (flip apply) game moves
   in case apply CastleKing =<< game' of
        Right g -> do
          findPieces King Black g `shouldBe` [PieceOnBoard King Black (Pos 7 6)]
          findPieces Rook Black g `shouldBe` [PieceOnBoard Rook Black (Pos 7 0), PieceOnBoard Rook Black (Pos 7 5)]
        Left e -> fail ("cannot apply castling for black on king side: " <> show e)

black_can_castle_queen_side :: Expectation
black_can_castle_queen_side =
  let game = initialGame
      moves =
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
          let game =
                Game
                  side
                  NoCheck
                  (PieceOnBoard piece (flipSide side) threat : castlingPosition side)
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
-- * Generic Properties

isIllegal :: Game -> Move -> Property
isIllegal game move =
  case apply move game of
    Right game' ->
      property False
        & counterexample ("after:\n" <> unpack (render game'))
        & counterexample ("before:\n" <> unpack (render game))
        & counterexample ("move: " <> show move)
    Left err ->
      err
        === IllegalMove move
        & counterexample ("game: " <> show game)

isBlocked :: Game -> Move -> Property
isBlocked game move =
  case apply move game of
    Right game' ->
      property False
        & counterexample ("after:\n" <> unpack (render game'))
        & counterexample ("before:\n" <> unpack (render game))
        & counterexample ("move: " <> show move)
    Left err ->
      err
        === MoveBlocked pos from to
        & counterexample ("game: " <> show game)
 where
  Move from to = move
  Just PieceOnBoard{pos} = game `firstPieceOn` path from to

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
