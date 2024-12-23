{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | A smart contract that controls a player's ELO rating and their
-- participation in games within Hydra.
--
-- The contract is parameterised by the player's public key so that it
-- can only be committed to a head by the controlling player.  It can
-- also be spent to start a new game, in which case it becomes locked
-- until the game ends.
module Chess.ELO where

import PlutusTx.Prelude

import Chess.Plutus (ValidatorType, scriptValidatorHash, validatorToBytes, wrapValidator)
import Data.ByteString (ByteString)
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  PubKeyHash,
  ScriptContext (..),
  ScriptHash,
  SerialisedScript,
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Value (getValue),
  getPubKeyHash,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2.Contexts (findOwnInput)
import PlutusPrelude (guard)
import PlutusTx (CompiledCode, compile, liftCode, unsafeApplyCode)
import PlutusTx.AssocMap (Map, lookup)

validator :: CurrencySymbol -> Integer -> BuiltinData -> ScriptContext -> Bool
validator tokenCurrency _eloScore _redeemer context =
  traceIfFalse "check signature matches token owner" (checkSignature tokenCurrency context)
{-# INLINEABLE validator #-}

checkSignature :: CurrencySymbol -> ScriptContext -> Bool
checkSignature currency context@ScriptContext{scriptContextTxInfo = TxInfo{txInfoSignatories}} =
  isJust findSignatory
 where
  findSignatory = do
    TxInInfo{txInInfoResolved} <- findOwnInput context
    value <- lookup currency $ getValue $ txOutValue txInInfoResolved
    traverse (findSignatory' value) txInfoSignatories

  findSignatory' :: Map TokenName Integer -> PubKeyHash -> Maybe PubKeyHash
  findSignatory' tokens signatory = do
    token <- lookup (TokenName $ getPubKeyHash signatory) tokens
    guard $ token == 1
    pure signatory
{-# INLINEABLE checkSignature #-}

compiledValidator :: CompiledCode (CurrencySymbol -> ValidatorType)
compiledValidator =
  $$(compile [||wrap . validator||])
 where
  wrap = wrapValidator @Integer @BuiltinData

validatorScript :: CurrencySymbol -> SerialisedScript
validatorScript currency =
  serialiseCompiledCode
    $ compiledValidator
    `unsafeApplyCode` PlutusTx.liftCode plcVersion100 currency

validatorHash :: CurrencySymbol -> ScriptHash
validatorHash = scriptValidatorHash . validatorScript

validatorBytes :: CurrencySymbol -> ByteString
validatorBytes = validatorToBytes . validatorScript
