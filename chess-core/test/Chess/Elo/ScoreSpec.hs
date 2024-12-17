module Chess.Elo.ScoreSpec where

import Chess.Elo.Score (Result (..), eloGain)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec =
  it "can compute Elo gains as a change for each player" $ do
    eloGain 1800 2005 AWin `shouldBe` 15
    eloGain 1800 2005 BWin `shouldBe` -5
    eloGain 1800 2005 Draw `shouldBe` 5
