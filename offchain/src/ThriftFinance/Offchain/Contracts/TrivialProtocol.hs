{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ThriftFinance.Offchain.Contracts.TrivialProtocol (
  lockFunds,
  unlockFunds,
) where

import Data.Map qualified as Map
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints.OffChain (ScriptLookups)
import Plutus.Contract (AsContractError, Contract, ContractError)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Api (Validator, toBuiltinData)
import Plutus.V1.Ledger.Scripts (MintingPolicy)

import Control.Monad (void)
import Ledger (txId)
import Ledger.Value (singleton)
import ThriftFinance.Common.Constants (trivialRewardTokenName, trivialTokenName)
import ThriftFinance.Offchain.Utils (validatorAddress, waitSomeTime)

{- | Lock `amount` of Lovelace in `validator` and mint for each Ada one token of
 | `trivialTokenMp`
-}
lockFunds :: forall r w. Integer -> Integer -> Validator -> MintingPolicy -> Contract r w ContractError ()
lockFunds lockedAmount mintedAmount validator trivialTokenMp = do
  utxos <- Contract.ownUtxos
  let -- No datum needed
      datum = Ledger.Datum $ toBuiltinData ()
      -- The locked and minted values are equal in quantity
      lockedValue = Ada.lovelaceValueOf lockedAmount
      mintedValue = singleton (scriptCurrencySymbol trivialTokenMp) trivialTokenName mintedAmount
      valHash = validatorHash validator
      constraints :: TxConstraints Void Void
      constraints =
        Constraints.mustPayToOtherScript valHash datum lockedValue
          <> Constraints.mustMintValue mintedValue
      lookups :: ScriptLookups Void
      lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript validator
          <> Constraints.mintingPolicy trivialTokenMp
  void $ Ledger.getCardanoTxId <$> Contract.submitTxConstraintsWith lookups constraints
  void waitSomeTime

{- | Unlock `amount` of Lovelace in `validator` and mint reward tokens.
 | To do that, burn one liquidity token for every 5 reward tokens minted.
-}
unlockFunds ::
  forall r w.
  Integer ->
  Integer ->
  Validator ->
  MintingPolicy ->
  MintingPolicy ->
  Contract r w ContractError ()
unlockFunds amountLocked amountMinted validator trivialTokenMp trivialRewardTokenMp = do
  utxos <- Contract.ownUtxos
  poolUtxos <- Contract.utxosAt $ validatorAddress validator
  let (lockedUtxo, _) = head $ Map.toList poolUtxos
      burntValue =
        singleton
          (scriptCurrencySymbol trivialTokenMp)
          trivialTokenName
          (-amountLocked)
      mintedValue =
        singleton
          (scriptCurrencySymbol trivialRewardTokenMp)
          trivialRewardTokenName
          amountMinted
  let constraints :: TxConstraints Void Void
      constraints =
        Constraints.mustSpendScriptOutput lockedUtxo (Ledger.Redeemer $ toBuiltinData ())
          <> Constraints.mustMintValue (mintedValue <> burntValue)
      lookups :: ScriptLookups Void
      lookups =
        Constraints.otherScript validator
          <> Constraints.mintingPolicy trivialTokenMp
          <> Constraints.mintingPolicy trivialRewardTokenMp
          <> Constraints.unspentOutputs (utxos <> poolUtxos)
  void $ Ledger.getCardanoTxId <$> Contract.submitTxConstraintsWith lookups constraints
  void waitSomeTime
