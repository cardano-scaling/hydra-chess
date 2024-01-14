{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Client.ConsoleSpec where

import Game.Client.Console (readInput, inputParser)
import Game.Client.IO (Command (..))
import Test.Hspec (Spec, it, shouldBe)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
  it "parses 'quit' command" $ do
    readInput inputParser "quit" `shouldBe` Right Quit
    readInput inputParser "q" `shouldBe` Right Quit

  it "parses 'play' command" $
    readInput inputParser ("play " <> " e2-e4") === Right (Play "e2-e4")

  it "parses 'newGame' command" $
    readInput inputParser "newGame " `shouldBe` Right NewGame

  it "parses 'stop' command" $
    readInput inputParser ("stop ") `shouldBe` Right Stop
