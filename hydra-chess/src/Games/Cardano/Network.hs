{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Cardano.Network where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Text (unpack)

data Network = Preview | Preprod | Mainnet
  deriving stock (Eq, Show, Read)

instance ToJSON Network where
  toJSON = \case
    Preview -> String "preview"
    Preprod -> String "preprod"
    Mainnet -> String "mainnet"

instance FromJSON Network where
  parseJSON = withText "Network" $ \case
    "preview" -> pure Preview
    "preprod" -> pure Preprod
    "mainnet" -> pure Mainnet
    other -> fail $ "Unknown network " <> unpack other

cardanoNodeVersion :: Network -> String
cardanoNodeVersion Preview = "10.1.3"
cardanoNodeVersion Preprod = "10.1.3"
cardanoNodeVersion Mainnet = "10.1.3"

networkDir :: Network -> FilePath
networkDir = \case
  Preview -> "preview"
  Preprod -> "preprod"
  Mainnet -> "mainnet"

networkMagicArgs :: Network -> [String]
networkMagicArgs = \case
  Preview -> ["--testnet-magic", "2"]
  Preprod -> ["--testnet-magic", "1"]
  Mainnet -> ["--mainnet"]
