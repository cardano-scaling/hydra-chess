{-# LANGUAGE OverloadedStrings #-}

module Games.Server.HydraSpec where

import qualified Data.Map as Map
import Game.Client.Console (Coin (..), Coins (..), SimpleUTxO (..), parseQueryUTxO)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "can parse UTxO from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f + TxOutDatumNone"
        expected =
          SimpleUTxO
            { txIn = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4#0"
            , coins =
                Coins 1200000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("666f6f", 1)])
                      )
                    ]
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected

  it "can parse UTxO without any datum from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f"
        expected =
          SimpleUTxO
            { txIn = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4#0"
            , coins =
                Coins 1200000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("666f6f", 1)])
                      )
                    ]
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected

  it "can parse UTxO with datum hash from cardano-cli to Hydra API Request" $ do
    let rawOutput = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0     0        10000000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98 + TxOutDatumHash ScriptDataInBabbageEra \"36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e\""
        expected =
          UTxOWithDatum
            { txIn = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0"
            , coins =
                Coins 10000000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98", 1)])
                      )
                    ]
            , datumHash = "36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e"
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected
