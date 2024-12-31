{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Chess.Contract where

import PlutusTx.Prelude

import Chess.Elo.Score (Result (..), eloGain)
import Chess.Game (Check (..), Game (..), Move, Side (..), apply)
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Plutus (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (Datum),
  OutputDatum (..),
  PubKeyHash (..),
  ScriptContext (..),
  ScriptHash (..),
  SerialisedScript,
  TokenName (..),
  TxInfo (..),
  TxOut (..),
  addressCredential,
  fromBuiltinData,
  getDatum,
  getValue,
  serialiseCompiledCode,
  toBuiltinData,
  txInfoSignatories,
  txOutDatum,
 )
import PlutusLedgerApi.V2.Contexts (findDatumHash, getContinuingOutputs)
import PlutusTx (CompiledCode, unsafeApplyCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List qualified as List

validator :: (CurrencySymbol, ScriptHash) -> ChessGame -> ChessPlay -> ScriptContext -> Bool
validator scripts chess play scriptContext =
  case play of
    ChessMove move -> checkMove move chess scriptContext
    End -> checkGameEnd scripts chess scriptContext
{-# INLINEABLE validator #-}

checkMove :: Move -> ChessGame -> ScriptContext -> Bool
checkMove move chess@ChessGame{players, game} scriptContext@ScriptContext{scriptContextTxInfo = txInfo} =
  isPlayersTurn game
    && case apply move game of
      Left{} -> traceError "Illegal move"
      Right game' -> checkGameOutput scriptContext chess{game = game'}
 where
  isPlayersTurn Game{curSide}
    | length players == 2 = checkPlayerTurn curSide
    | length players == 1 = True -- solo mode
    | otherwise = traceError "Number of players must be 1 or 2"

  checkPlayerTurn side =
    case txInfoSignatories txInfo of
      [signer] ->
        case findIndex (\(pkh, _) -> pkh == signer) players of
          Just idx ->
            traceIfFalse "Wrong side to play"
              $ (idx == 0 && side == White)
              || (idx == 1 && side == Black)
          Nothing -> traceError "Wrong signer"
      [] ->
        traceError "No signers"
      _ ->
        traceError "Too many signers"
{-# INLINEABLE checkMove #-}

checkGameOutput :: ScriptContext -> ChessGame -> Bool
checkGameOutput ctx d =
  case ownDatum of
    NoOutputDatum ->
      traceError "missing datum"
    OutputDatumHash actualHash ->
      traceIfFalse
        "output datum hash mismatch"
        ( Just actualHash == expectedHash
        )
    OutputDatum actual ->
      traceIfFalse "output datum mismatch" $ getDatum actual == expectedData
 where
  expectedData = toBuiltinData d

  expectedHash = findDatumHash (Datum expectedData) txInfo

  ownDatum =
    case getContinuingOutputs ctx of
      [o] -> txOutDatum o
      _ -> traceError "expected only one head output"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkGameOutput #-}

-- | Verifies game is ending correctly and players get rewarded accordingly.
checkGameEnd :: (CurrencySymbol, ScriptHash) -> ChessGame -> ScriptContext -> Bool
checkGameEnd scripts ChessGame{players, game = Game{checkState}} ctx =
  case checkState of
    CheckMate side -> checkEloChange scripts side players ctx
    Resigned side -> checkEloChange scripts side players ctx
    _other -> trace "game not ended" False
{-# INLINEABLE checkGameEnd #-}

-- | Verifies that transaction correctly reports Elo changes to player's
-- game tokens' output.
--
-- We need to:
-- * Find the output paying to elo script
-- * Check which player it's for
-- * Compute the Elo change Δ for winner and loser
-- * Check the new datum for the outputs is the old Elo +/- Δ depending on win/lose
-- * Check the output's value contains the game token + 2₳ deposits
checkEloChange :: (CurrencySymbol, ScriptHash) -> Side -> [(PubKeyHash, Integer)] -> ScriptContext -> Bool
checkEloChange (pid, eloScriptHash) losingSide players ctx =
  case players of
    [w, b] ->
      fromMaybe False $ checkEloChangeForPlayer w b
    [_] -> True -- FIXME: check Elo is not changed
    _other -> trace "invalid number of players" False
 where
  checkEloChangeForPlayer (whitePkh, whiteElo) (blackPkh, blackElo) = do
    TxOut{txOutValue, txOutDatum} <- scriptOutput
    toks <- Map.lookup pid (getValue txOutValue)
    pkh <- case Map.toList toks of
      [(p, v)] | v == 1 -> Just p
      _other -> Nothing
    actualElo <- getActualElo txOutDatum >>= fromBuiltinData
    side <- playerSide $ PubKeyHash $ unTokenName pkh
    case side of
      White -> Just $ actualElo == whiteElo + eloChange
      Black -> Just $ actualElo == blackElo + eloChange
   where
    eloChange =
      eloGain
        whiteElo
        blackElo
        ( if losingSide == White
            then BWin
            else AWin
        )
    playerSide pkh
      | pkh == whitePkh = Just White
      | pkh == blackPkh = Just Black
      | otherwise = Nothing

    getActualElo txOutDatum =
      case txOutDatum of
        OutputDatum newElo -> Just $ getDatum newElo
        _other -> Nothing

    isEloScript TxOut{txOutAddress} = addressCredential txOutAddress == ScriptCredential eloScriptHash

    scriptOutput = List.find isEloScript txInfoOutputs

  ScriptContext{scriptContextTxInfo = TxInfo{txInfoOutputs}} = ctx
{-# INLINEABLE checkEloChange #-}

compiledValidator :: CompiledCode ((CurrencySymbol, ScriptHash) -> ValidatorType)
compiledValidator =
  $$(PlutusTx.compile [||wrap . validator||])
 where
  wrap = wrapValidator @ChessGame @ChessPlay

validatorScript :: (CurrencySymbol, ScriptHash) -> SerialisedScript
validatorScript scripts =
  serialiseCompiledCode
    $ compiledValidator
    `unsafeApplyCode` PlutusTx.liftCode plcVersion100 scripts

validatorHash :: (CurrencySymbol, ScriptHash) -> ScriptHash
validatorHash = scriptValidatorHash . validatorScript
