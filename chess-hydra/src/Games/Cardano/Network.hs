{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Games.Cardano.Network where

data Network = Preview | Preprod | Mainnet
  deriving stock (Eq, Show, Read)

cardanoNodeVersion :: Network -> String
cardanoNodeVersion Preview = "8.7.1"
cardanoNodeVersion Preprod = "8.7.1"
cardanoNodeVersion Mainnet = "8.7.1"

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
