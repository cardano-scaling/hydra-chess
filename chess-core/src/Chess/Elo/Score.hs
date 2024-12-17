{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fno-strictness -fno-spec-constr #-}

module Chess.Elo.Score where

import PlutusTx.Prelude
import Prelude qualified as Haskell

data Result = AWin | BWin | Draw
  deriving stock (Haskell.Eq, Haskell.Show)

-- | A list of probabilities of winning for the first player given
-- a difference in Elo from -400 to 400. The list is encoded such that
-- the first element is the number of identical results, the second
-- and third are numberator and denominator of the probability of winning.
--
-- To find the probability given a difference `d`, look up the first element
-- in this list such that the accumulated number is greater than `d`, and
-- return the probability as a Rational.
probaWin :: [(Integer, Integer, Integer)]
probaWin =
  [ (-382, 9, 100)
  , (-364, 1, 10)
  , (-347, 11, 100)
  , (-331, 3, 25)
  , (-316, 13, 100)
  , (-302, 7, 50)
  , (-289, 3, 20)
  , (-276, 4, 25)
  , (-264, 17, 100)
  , (-252, 9, 50)
  , (-241, 19, 100)
  , (-231, 1, 5)
  , (-220, 21, 100)
  , (-210, 11, 50)
  , (-201, 23, 100)
  , (-191, 6, 25)
  , (-182, 1, 4)
  , (-173, 13, 50)
  , (-165, 27, 100)
  , (-148, 7, 25)
  , (-139, 3, 10)
  , (-131, 31, 100)
  , (-124, 8, 25)
  , (-116, 33, 100)
  , (-108, 17, 50)
  , (-100, 7, 20)
  , (-93, 9, 25)
  , (-86, 37, 100)
  , (-78, 19, 50)
  , (-71, 39, 100)
  , (-64, 2, 5)
  , (-57, 41, 100)
  , (-49, 21, 50)
  , (-42, 43, 100)
  , (-35, 11, 25)
  , (-28, 9, 20)
  , (-21, 23, 50)
  , (-14, 47, 100)
  , (-7, 12, 25)
  , (-1, 49, 100)
  , (6, 1, 2)
  , (13, 51, 100)
  , (20, 13, 25)
  , (27, 53, 100)
  , (34, 27, 50)
  , (41, 11, 20)
  , (56, 14, 25)
  , (63, 57, 100)
  , (70, 59, 100)
  , (77, 3, 5)
  , (85, 61, 100)
  , (92, 31, 50)
  , (99, 63, 100)
  , (107, 16, 25)
  , (115, 13, 20)
  , (123, 33, 50)
  , (130, 67, 100)
  , (138, 17, 25)
  , (147, 69, 100)
  , (155, 7, 10)
  , (164, 71, 100)
  , (172, 18, 25)
  , (181, 73, 100)
  , (190, 37, 50)
  , (200, 3, 4)
  , (209, 19, 25)
  , (219, 77, 100)
  , (230, 39, 50)
  , (240, 79, 100)
  , (251, 4, 5)
  , (263, 81, 100)
  , (275, 41, 50)
  , (288, 83, 100)
  , (301, 21, 25)
  , (315, 17, 20)
  , (330, 43, 50)
  , (346, 87, 100)
  , (363, 22, 25)
  , (381, 89, 100)
  , (400, 9, 10)
  ]
{-# INLINEABLE probaWin #-}

-- | Compute the Elo gain for a player given their current Elo and the result of the game.
-- This computation is based on the FIDE Elo rating system as described
-- on [Wikipedia](https://en.wikipedia.org/wiki/Elo_rating_system).
eloGain :: Integer -> Integer -> Result -> Integer
eloGain aScore bScore result =
  let diff = aScore - bScore
      expected = expectedResult diff probaWin
   in round (fromInteger 20 * (fromResult result - expected))
{-# INLINEABLE eloGain #-}

expectedResult :: Integer -> [(Integer, Integer, Integer)] -> Rational
expectedResult diff = go
 where
  go [] = fromInteger 1
  go ((n, num, den) : rest)
    | diff < n = num `unsafeRatio` den
    | otherwise = go rest
{-# INLINEABLE expectedResult #-}

fromResult :: Result -> Rational
fromResult = \case
  AWin -> fromInteger 1
  BWin -> fromInteger 0
  Draw -> 1 `unsafeRatio` 2
{-# INLINEABLE fromResult #-}
