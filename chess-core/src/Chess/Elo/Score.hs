{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fno-strictness -fno-spec-constr #-}

module Chess.Elo.Score where

import PlutusTx.Prelude

data Result = AWin | BWin | Draw

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
  [ (19, 9, 100)
  , (18, 1, 10)
  , (17, 11, 100)
  , (16, 3, 25)
  , (15, 13, 100)
  , (14, 7, 50)
  , (13, 3, 20)
  , (13, 4, 25)
  , (12, 17, 100)
  , (12, 9, 50)
  , (11, 19, 100)
  , (10, 1, 5)
  , (11, 21, 100)
  , (10, 11, 50)
  , (9, 23, 100)
  , (10, 6, 25)
  , (9, 1, 4)
  , (9, 13, 50)
  , (8, 27, 100)
  , (17, 7, 25)
  , (9, 3, 10)
  , (8, 31, 100)
  , (7, 8, 25)
  , (8, 33, 100)
  , (8, 17, 50)
  , (8, 7, 20)
  , (7, 9, 25)
  , (7, 37, 100)
  , (8, 19, 50)
  , (7, 39, 100)
  , (7, 2, 5)
  , (7, 41, 100)
  , (8, 21, 50)
  , (7, 43, 100)
  , (7, 11, 25)
  , (7, 9, 20)
  , (7, 23, 50)
  , (7, 47, 100)
  , (7, 12, 25)
  , (6, 49, 100)
  , (7, 1, 2)
  , (7, 51, 100)
  , (7, 13, 25)
  , (7, 53, 100)
  , (7, 27, 50)
  , (7, 11, 20)
  , (15, 14, 25)
  , (7, 57, 100)
  , (7, 59, 100)
  , (7, 3, 5)
  , (8, 61, 100)
  , (7, 31, 50)
  , (7, 63, 100)
  , (8, 16, 25)
  , (8, 13, 20)
  , (8, 33, 50)
  , (7, 67, 100)
  , (8, 17, 25)
  , (9, 69, 100)
  , (8, 7, 10)
  , (9, 71, 100)
  , (8, 18, 25)
  , (9, 73, 100)
  , (9, 37, 50)
  , (10, 3, 4)
  , (9, 19, 25)
  , (10, 77, 100)
  , (11, 39, 50)
  , (10, 79, 100)
  , (11, 4, 5)
  , (12, 81, 100)
  , (12, 41, 50)
  , (13, 83, 100)
  , (13, 21, 25)
  , (14, 17, 20)
  , (15, 43, 50)
  , (16, 87, 100)
  , (17, 22, 25)
  , (18, 89, 100)
  , (19, 9, 10)
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
expectedResult diff = go (-400)
 where
  go _ [] = fromInteger 1
  go d ((n, num, den) : rest)
    | diff < d = num `unsafeRatio` den
    | otherwise = go (d + n) rest
{-# INLINEABLE expectedResult #-}

fromResult :: Result -> Rational
fromResult = \case
  AWin -> fromInteger 1
  BWin -> fromInteger 0
  Draw -> 1 `unsafeRatio` 2
{-# INLINEABLE fromResult #-}
