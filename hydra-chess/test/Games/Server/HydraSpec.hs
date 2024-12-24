{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Games.Server.HydraSpec where

import qualified Chess.Game as Chess
import Chess.GameState (ChessGame (..))
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Game.Client.Console (Coin (..), Coins (..))
import Game.Server (Host (..))
import Games.Run.Hydra (Peer (..))
import Games.Server.JSON (FullUTxO (..), UTxOs (..), extractGameState, extractGameToken)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "parses Coins" $ do
    let inp :: LBS.ByteString = "{       \"5152458e042159beca8f5efe14e4848444a5ead9c49cf3d389f449f5\": {         \"920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b18\": 1       },       \"lovelace\": 2000000     }"
    eitherDecode inp
      `shouldBe` Right
        ( Coins
            2000000
            ( Map.fromList
                [
                  ( "5152458e042159beca8f5efe14e4848444a5ead9c49cf3d389f449f5"
                  , Coin $ Map.fromList [("920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b18", 1)]
                  )
                ]
            )
        )

  it "parses UTxOs from cardano-cli output" $ do
    let utxos = eitherDecode utxoFromCli
    utxos
      `shouldBe` Right
        ( UTxOs
            [ FullUTxO
                { txIn = "1b0ea1b1db9beca2e74d984f49063bf3b092f935ad48fbdaf5c4b6504636ba49#1"
                , address = "addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6"
                , value = Coins 3347361 mempty
                , datum = Nothing
                , datumHash = Nothing
                , inlineDatum = Nothing
                , inlineDatumRaw = Nothing
                , inlineDatumHash = Nothing
                , scriptInfo = Nothing
                }
            , FullUTxO
                { txIn = "c3eb0231fc9da50f29e4964d1f73c0fa6c01234e26b9788365fdebc8c2f26c97#1"
                , address = "addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6"
                , value = Coins 8915180 mempty
                , datum = Nothing
                , datumHash = Nothing
                , inlineDatum = Nothing
                , inlineDatumRaw = Nothing
                , inlineDatumHash = Nothing
                , scriptInfo = Nothing
                }
            , FullUTxO
                { txIn = "cd536c0877458dde3ba0b3ee6cbb90e5c1ae904ad3fe9304f154d801312bf5f5#0"
                , address = "addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6"
                , value = Coins 14562790 mempty
                , datum = Nothing
                , datumHash = Nothing
                , inlineDatum = Nothing
                , inlineDatumRaw = Nothing
                , inlineDatumHash = Nothing
                , scriptInfo = Nothing
                }
            ]
        )

  it "decodes peers JSON file" $ do
    let peersJSON = "[{\"name\":\"arnaud-mbp\",\"address\":{\"host\":\"192.168.1.103\",\"port\":5551},\"hydraKey\":\"58206197ef01d4b8afab12850b1d0f3245b6b196c067dc17f2f86a001285b535a720\",\"cardanoKey\":\"5820b8e0dfdb94e6b1fbadd8cd70756d995664d3cc41d0bd52ec0a1e982f711ec5a1\"}]"

    Aeson.eitherDecode' peersJSON
      `shouldBe` Right
        [ Peer
            { name = "arnaud-mbp"
            , address = Host "192.168.1.103" 5551
            , hydraKey = "58206197ef01d4b8afab12850b1d0f3245b6b196c067dc17f2f86a001285b535a720"
            , cardanoKey = "5820b8e0dfdb94e6b1fbadd8cd70756d995664d3cc41d0bd52ec0a1e982f711ec5a1"
            }
        ]

  it "extracts game token from JSON UTxO" $ do
    let utxo = fromJust $ Aeson.decode "{\"c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0\":{\"address\":\"addr_test1wz4y5mkg3m83dh3npqygnzst74s26cewjw3uel2ylcuqagg9zad83\",\"datum\":null,\"inlineDatumhash\":\"36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e\",\"inlineDatum\":{\"int\": 1000},\"inlineDatumRaw\":\"1903e8\",\"referenceScript\":null,\"value\":{\"e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10\":{\"1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98\":1},\"lovelace\":10000000}}}"

    extractGameToken "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10" "1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98" utxo
      `shouldBe` Just "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0"

  it "extracts game state from UTxO" $ do
    let utxo = fromJust $ Aeson.decode snapshotConfirmed
    extractGameState "addr_test1wz0qkt24z4qjtrzyq42nd4dfy3822d2pylj9n5jwnqplw5q6gh6ez" utxo
      `shouldBe` Right (ChessGame [("920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b18", 1666)] Chess.initialGame)

snapshotConfirmed :: LBS.ByteString
snapshotConfirmed = "{\"f37e113e9b76ce2d373640d479a94ef6717f7273c81add1ce258aaac4e6562a5#0\":{\"address\":\"addr_test1wz0qkt24z4qjtrzyq42nd4dfy3822d2pylj9n5jwnqplw5q6gh6ez\",\"datum\":null,\"inlineDatum\":{\"constructor\":0,\"fields\":[{\"list\":[{\"constructor\":0,\"fields\":[{\"bytes\":\"920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b18\"},{\"int\":1666}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"list\":[{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":4,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":4,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":5,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":5,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":4}]}]}]},{\"list\":[]}]}]},\"inlineDatumRaw\":\"d8799f9fd8799f581c920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b1800ffffd8799fd87980d879809fd8799fd87980d87980d8799f0100ffffd8799fd87980d87980d8799f0101ffffd8799fd87980d87980d8799f0102ffffd8799fd87980d87980d8799f0103ffffd8799fd87980d87980d8799f0104ffffd8799fd87980d87980d8799f0105ffffd8799fd87980d87980d8799f0106ffffd8799fd87980d87980d8799f0107ffffd8799fd87980d87a80d8799f0600ffffd8799fd87980d87a80d8799f0601ffffd8799fd87980d87a80d8799f0602ffffd8799fd87980d87a80d8799f0603ffffd8799fd87980d87a80d8799f0604ffffd8799fd87980d87a80d8799f0605ffffd8799fd87980d87a80d8799f0606ffffd8799fd87980d87a80d8799f0607ffffd8799fd87a80d87a80d8799f0700ffffd8799fd87a80d87a80d8799f0707ffffd8799fd87a80d87980d8799f0000ffffd8799fd87a80d87980d8799f0007ffffd8799fd87c80d87a80d8799f0701ffffd8799fd87c80d87a80d8799f0706ffffd8799fd87c80d87980d8799f0001ffffd8799fd87c80d87980d8799f0006ffffd8799fd87b80d87a80d8799f0702ffffd8799fd87b80d87a80d8799f0705ffffd8799fd87b80d87980d8799f0002ffffd8799fd87b80d87980d8799f0005ffffd8799fd87d80d87a80d8799f0703ffffd8799fd87d80d87980d8799f0003ffffd8799fd87e80d87a80d8799f0704ffffd8799fd87e80d87980d8799f0004ffffff80ffff\",\"inlineDatumhash\":\"9a2a457b34c70f39515d0cd8320210f97cd7f1933c25e62470d030fe9b3e8efc\",\"referenceScript\":null,\"value\":{\"5152458e042159beca8f5efe14e4848444a5ead9c49cf3d389f449f5\":{\"920e34e143094e3cb33f5a314db4d9d8f5a254e96cd6e699ecbf1b18\":1},\"lovelace\":2000000}},\"f37e113e9b76ce2d373640d479a94ef6717f7273c81add1ce258aaac4e6562a5#1\":{\"address\":\"addr_test1vzfqud8pgvy5u09n8adrznd5m8v0tgj5a9kdde5eajl3kxqwh7f8c\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"inlineDatumRaw\":null,\"referenceScript\":null,\"value\":{\"lovelace\":8000000}}}"

utxoFromCli :: LBS.ByteString
utxoFromCli = "{ \"1b0ea1b1db9beca2e74d984f49063bf3b092f935ad48fbdaf5c4b6504636ba49#1\": { \"address\": \"addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6\", \"datum\": null, \"datumhash\": null, \"inlineDatum\": null, \"inlineDatumRaw\": null, \"referenceScript\": null, \"value\": { \"lovelace\": 3347361 } }, \"c3eb0231fc9da50f29e4964d1f73c0fa6c01234e26b9788365fdebc8c2f26c97#1\": { \"address\": \"addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6\", \"datum\": null, \"datumhash\": null, \"inlineDatum\": null, \"inlineDatumRaw\": null, \"referenceScript\": null, \"value\": { \"lovelace\": 8915180 } }, \"cd536c0877458dde3ba0b3ee6cbb90e5c1ae904ad3fe9304f154d801312bf5f5#0\": { \"address\": \"addr_test1vzvtjwtqpvaaknyv7zugpjt7w7hgw7k29hujlxu0uy9gtwchan3c6\", \"datum\": null, \"datumhash\": null, \"inlineDatum\": null, \"inlineDatumRaw\": null, \"referenceScript\": null, \"value\": { \"lovelace\": 14562790 } }}"
