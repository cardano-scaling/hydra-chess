{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.Elo.ScoreSpec where

import Chess.Elo.Score (Result (..), eloGain)
import Data.Function ((&))
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, choose, counterexample, elements, forAll, suchThat, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (..))

spec :: Spec
spec = do
  it "can compute Elo gains as a change for each player" $ do
    eloGain 1800 2005 AWin `shouldBe` 15
    eloGain 1800 2005 BWin `shouldBe` -5
    eloGain 1800 2005 Draw `shouldBe` 5

  prop "gains are (nearly) symetric for wins with less than 400 margin" $
    forAll arbitrary $ \(Elo a) ->
      forAll (arbitrary `suchThat` \(Elo b) -> abs (a - b) < 400) $ \(Elo b) ->
        abs (eloGain a b AWin + eloGain b a BWin) <= 1
          & counterexample ("a wins: " <> show (eloGain a b AWin) <> ", b wins: " <> show (eloGain b a BWin))

  prop "difference between Elos is capped at 400" $
    forAll arbitrary $ \(Elo a) ->
      forAll (choose (400, 700)) $ \b ->
        eloGain a (a + b) AWin === eloGain a (a + 400) AWin

newtype Elo = Elo Integer
  deriving (Eq, Show)

instance Arbitrary Elo where
  arbitrary = Elo <$> elements [1000 .. 3000]
  shrink (Elo x)
    | x > 1000 = Elo <$> shrink x
    | otherwise = []

instance Arbitrary Result where
  arbitrary = elements [AWin, BWin, Draw]
