module ThriftFinance.Offchain.Contracts.Trigger (
  triggerPool,
) where

import ThriftFinance.Offchain.Error (
  OtherError (CouldNotGetTargetPolicies),
  TFError (ConstraintErr, OtherErr, TriggerPoolErr),
  TriggerPoolError (BadAmountOfFunds, BadNumberOfContributions, expectedNumber, expectedValue, number, value),
 )

import ThriftFinance.Offchain.TFContract (
  TFContract,
  getTPInfo,
  getTfTkMp,
  getTfTokenCs,
  getTfValidator,
  getTfValidatorAddress,
  getTpPolicies,
  getTpValidator,
  logInfo,
 )

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Void (Void)

import Ledger (Value, scriptCurrencySymbol)
import Ledger.Constraints (UnbalancedTx)
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (throwError)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)

import ThriftFinance.Common.Types (
  DepositState (ActiveDepositState),
  Deposits (unDeposits),
  TFRedeemer (Activate),
  TrivialTkMpRedeemer (MintTk),
  toList,
  tpi'DepositTarget,
  tpi'NumberOfContributions,
  tpi'PassiveDepositCurrency,
  tpi'PassiveDepositName,
 )

import ThriftFinance.Common.Constants (
  tfActiveTokenName,
  tfDepositTokenName,
  trivialTokenName,
 )

import ThriftFinance.Offchain.Check (Check, mkCheck, runCheck)
import ThriftFinance.Offchain.Utils (
  getAmountDeposited,
  getFeeConstraint,
  getFirstOwnWalletAddress,
  getTfPendingUtxos,
  getValidatorAdressHash,
  updateDepositState,
  valueOf',
  wrapIntoDatum,
  wrapIntoRedeemer,
 )

import Plutus.Contract.Request qualified as Contract

import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)

{- | This check returns successfully if the following conditions are satisfied
 | 1. The number of contributions matches the target in TargetProtocolInfo
 | 2. The pooled funds quantity matches the target in TargetProtocolInfo
-}
checkTriggerConditions :: Deposits -> Value -> Check
checkTriggerConditions deposits totalDeposited = mkCheck $ do
  tpInfo <- getTPInfo
  tfTokenCs <- getTfTokenCs
  let targetContributions = tpi'NumberOfContributions tpInfo
      noContributions =
        foldl
          (\acc (_, (_, n)) -> acc + n)
          0
          $ toList (unDeposits deposits)
      targetValue =
        Value.singleton
          (tpi'PassiveDepositCurrency tpInfo)
          (tpi'PassiveDepositName tpInfo)
          (tpi'DepositTarget tpInfo)
          -- There should also be a DepositToken for every contribution made
          <> Value.singleton tfTokenCs tfDepositTokenName noContributions
  when (targetContributions /= noContributions) $
    throwError $
      TriggerPoolErr
        BadNumberOfContributions
          { expectedNumber = targetContributions
          , number = noContributions
          }
  when (targetValue /= totalDeposited) $
    throwError $
      TriggerPoolErr
        BadAmountOfFunds
          { expectedValue = targetValue
          , value = totalDeposited
          }

{- | Trigger the submission of a Thrift Finance pool - when the conditions have
 been met.
-}
triggerPool :: TFContract UnbalancedTx
triggerPool = do
  logInfo "triggerPool"
  walletAddr <- getFirstOwnWalletAddress
  walletFunds <- lift $ Contract.utxosAt walletAddr
  -- Retrieve TF information
  tfTokenCs <- getTfTokenCs
  tfTkMintingPolicy <- getTfTkMp
  tfAddress <- getTfValidatorAddress
  tfAddressHash <- getValidatorAdressHash tfAddress
  tfValidator <- getTfValidator
  -- Retrieve target protocol's validator and minting policies
  tpInfo <- getTPInfo
  tpValidator <- getTpValidator
  let tpValidatorAddress = mkValidatorAddress tpValidator
  tpAddressHash <- getValidatorAdressHash tpValidatorAddress
  tpTkMintingPolicy <-
    getTpPolicies >>= \case
      [tok, _rewardTok] -> pure tok
      _ -> throwError $ OtherErr CouldNotGetTargetPolicies
  let tpCs :: CurrencySymbol
      tpCs = scriptCurrencySymbol tpTkMintingPolicy
      tpTokenName :: TokenName
      tpTokenName = trivialTokenName
  -- Get pool's UTXO, total value and pending deposit information
  (txOutRef, txOut, deposits) :| _ <- getTfPendingUtxos
  depositedValue <- getAmountDeposited txOut
  -- Validate conditions before executing
  runCheck $ checkTriggerConditions deposits depositedValue
  -- We mint an active token and burn all deposit tokens
  let depositTokensLocked :: Integer
      depositTokensLocked = valueOf' tfTokenCs tfDepositTokenName depositedValue
      adaLockedAmount = tpi'DepositTarget tpInfo
      -- Burn all deposit TF tokens
      newDepositTokensBurnt = assetClassValue (assetClass tfTokenCs tfDepositTokenName) (-depositTokensLocked)
      -- Mint an active TF token
      newActiveTokenMinted = assetClassValue (assetClass tfTokenCs tfActiveTokenName) 1
      -- Mint 1:1 Trivial tokens for every Ada locked
      newTrivialTokensMinted = assetClassValue (assetClass tpCs tpTokenName) adaLockedAmount
      -- The deposited funds are locked in the target protocol
      tpValueLocked =
        Value.singleton
          (tpi'PassiveDepositCurrency tpInfo)
          (tpi'PassiveDepositName tpInfo)
          $ tpi'DepositTarget tpInfo
      -- The minted TF / TP tokens are locked in the validator.
      -- FIXME: Some amount of ADA is added to avoid triggering a
      -- BabbageOutputTooSmallUTxO error in Plutip
      tfValueLocked = newActiveTokenMinted <> newTrivialTokensMinted <> Ada.adaValueOf 10
  -- We update the deposit state to active while keeping all the contributions
  datum <- updateDepositState ActiveDepositState deposits

  feeConstraint <- getFeeConstraint

  let lookups =
        mconcat
          [ -- TF Minting Policy - for TF Token.
            Constraints.mintingPolicy tfTkMintingPolicy
          , -- TP Minting Policy - for liquidity token
            Constraints.mintingPolicy tpTkMintingPolicy
          , -- Wallet Funds.
            Constraints.unspentOutputs walletFunds
          , -- Pool UTXO
            Constraints.unspentOutputs $ Map.singleton txOutRef txOut
          , -- TF Validator Script.
            Constraints.otherScript tfValidator
          , -- TP Validator script
            Constraints.otherScript tpValidator
          ]

  let constraints =
        mconcat
          [ -- Spend TF Pool UTxO
            Constraints.mustSpendScriptOutput
              txOutRef
              (wrapIntoRedeemer ())
          , -- Datum must be correct.
            Constraints.mustIncludeDatum datum
          , -- Minted/burnt TFTokens.
            Constraints.mustMintValueWithRedeemer
              (wrapIntoRedeemer Activate)
              (newActiveTokenMinted <> newDepositTokensBurnt)
          , -- Minted TP tokens
            Constraints.mustMintValueWithRedeemer
              (wrapIntoRedeemer MintTk)
              newTrivialTokensMinted
          , -- Funds that must be Paid to the TFAddress.
            Constraints.mustPayToOtherScript
              tfAddressHash
              datum
              tfValueLocked
          , -- Funds that must be paid to the target protocol
            Constraints.mustPayToOtherScript
              tpAddressHash
              (wrapIntoDatum ())
              tpValueLocked
          , feeConstraint
          ]
  case Constraints.mkTx @Void lookups constraints of
    Left e ->
      throwError $ ConstraintErr e
    Right x ->
      pure x
