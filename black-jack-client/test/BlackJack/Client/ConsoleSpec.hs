{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BlackJack.Client.ConsoleSpec where

import BlackJack.Client.Console (readInput)
import BlackJack.Client.IO (Command (..))
import BlackJack.ClientSpec (KnownParties (KnownParties))
import BlackJack.Server (partyId)
import BlackJack.Server.Mock (MockChain)
import qualified Data.Text as Text
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  it "parses 'quit' command" $ do
    readInput "quit" `shouldBe` Right Quit
    readInput "q" `shouldBe` Right Quit

  prop "parses 'newTable' command" $ \(KnownParties parties) ->
    let names = (partyId @MockChain <$> parties)
     in readInput ("newTable " <> Text.unwords names) `shouldBe` Right (NewTable names)
