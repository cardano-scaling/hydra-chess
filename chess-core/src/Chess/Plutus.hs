{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Chess.Plutus (module Chess.Plutus, ToData)
where

import PlutusTx.Prelude

import Cardano.Binary (serialize')
import Crypto.Hash (Blake2b_224, hash)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Hex
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  PubKeyHash (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  UnsafeFromData,
 )
import PlutusTx (UnsafeFromData (..))
import PlutusTx qualified
import Prelude qualified

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
wrapValidator f d r c =
  check $ f datum redeemer context
 where
  datum = unsafeFromBuiltinData d
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE wrapValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> BuiltinUnit

data MintAction = Mint | Burn

PlutusTx.unstableMakeIsData ''MintAction

wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
wrapMintingPolicy f r c =
  check $ f redeemer context
 where
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE wrapMintingPolicy #-}

-- * Similar utilities as plutus-ledger

-- | Compute the on-chain 'ScriptHash' for a given serialised plutus script. Use
-- this to refer to another validator script.
scriptValidatorHash :: SerialisedScript -> ScriptHash
scriptValidatorHash =
  ScriptHash
    . toBuiltin @ByteString
    . convert
    . hash @_ @Blake2b_224
    . ( \sbs ->
          "\x02" -- the babbageScriptPrefixTag defined in cardano-ledger for PlutusV2 scripts
            Prelude.<> fromShort sbs
      )

-- | Encodes a compiled `PlutusScriptV2` validator into a representation suitable for cardano-cli.
--
-- This function is intended to be used to provide the representation
-- of a script to be stored in a file and passed as
-- `--tx-out-script-file` or `--mint-script-file` to the
-- `cardano-cli`.
validatorToBytes :: ShortByteString -> ByteString
validatorToBytes script =
  Lazy.toStrict
    . Aeson.encode
    $ object
      [ "type" .= ("PlutusScriptV2" :: Text)
      , "description" .= ("Chess Script" :: Text)
      , "cborHex" .= Text.decodeUtf8 (Hex.encode $ serialize' script)
      ]

pubKeyHash :: ByteString -> PubKeyHash
pubKeyHash h = PubKeyHash (toBuiltin h)

pubKeyHashToHex :: PubKeyHash -> Text
pubKeyHashToHex (PubKeyHash bibs) =
  Text.decodeUtf8 $ Hex.encode $ fromBuiltin bibs

pubKeyHashFromHex :: Text -> PubKeyHash
pubKeyHashFromHex hex = PubKeyHash (toBuiltin bytes)
 where
  bytes :: ByteString
  bytes = case Hex.decode $ Text.encodeUtf8 hex of
    Left err -> Prelude.error $ "Fail to decode bytestring from hex " <> Text.unpack hex <> ": " <> err
    Right v -> v

currencyFromBytes :: ByteString -> CurrencySymbol
currencyFromBytes bytes = CurrencySymbol (toBuiltin bytes)

scriptHashToBytes :: ScriptHash -> ByteString
scriptHashToBytes (ScriptHash bibs) = fromBuiltin bibs
