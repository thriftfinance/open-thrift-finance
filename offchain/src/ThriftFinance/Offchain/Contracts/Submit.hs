module ThriftFinance.Offchain.Contracts.Submit (
  submitContribution,
) where

import Data.Void (Void)

import ThriftFinance.Offchain.Error (SubmitPoolError (NoPoolsToContribute), TFError (ConstraintErr, SubmitPoolErr))
import ThriftFinance.Offchain.TFContract (TFContract, getTPInfo, getTfTkMp, getTfTokenCs, getTfValidator, getTfValidatorAddress, logInfo)

import Plutus.Contract qualified as Contract

import Ledger.Constraints (UnbalancedTx)
import Ledger.Constraints qualified as Constraints

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map

import Control.Monad.Reader (lift)
import Ledger.Value (assetClass, assetClassValue)
import ThriftFinance.Common.Constants (tfDepositTokenName)
import ThriftFinance.Common.Types (DepositState (PendingDepositState), Deposits (unDeposits), TFRedeemer (Deposit), TargetProtocolInfo (tpi'NumberOfContributions), toList)
import ThriftFinance.Offchain.Utils (
  addDeposit,
  getAmountDeposited,
  getDepositValue,
  getFeeConstraint,
  getFirstOwnWalletAddress,
  getTfPendingUtxos,
  getValidatorAdressHash,
  updateDepositState,
  wrapIntoRedeemer,
 )

-- | Submit a contribution to the a Thrift Finance peer lending pool.
submitContribution :: TFContract UnbalancedTx
submitContribution = do
  logInfo "submitContribution"
  -- Get wallet information
  walletAddr <- getFirstOwnWalletAddress
  walletFunds <- lift $ Contract.utxosAt walletAddr
  -- Retrieve TF information
  tfTokenCs <- getTfTokenCs
  tfTkMintingPolicy <- getTfTkMp
  tfAddress <- getTfValidatorAddress
  tfAddressHash <- getValidatorAdressHash tfAddress
  tfValidator <- getTfValidator
  tfDeposit <- getDepositValue
  tpInfo <- getTPInfo
  -- Get the TF pool's UTXOs and filter out pools without any contributions
  -- remaining. Obtain the total deposited in the pool
  pendingUtxos <- NonEmpty.toList <$> getTfPendingUtxos
  logInfo $ "(submitContribution) found these pools with pending state: " <> show pendingUtxos
  (tfOutRef, tfOut, tfDeposits) <-
    let withContributionsLeft :: (_, _, Deposits) -> Bool
        withContributionsLeft (_, _, deposits) =
          (toInteger . length . toList . unDeposits $ deposits) < tpi'NumberOfContributions tpInfo
     in case filter withContributionsLeft pendingUtxos of
          (x : _) -> pure x
          [] -> Contract.throwError $ SubmitPoolErr NoPoolsToContribute
  tfTotalDeposited <- getAmountDeposited tfOut
  let newDepositTokenMinted = assetClassValue (assetClass tfTokenCs tfDepositTokenName) 1
      -- The minted TF token and the deposit value are locked in the validator
      tfValueLocked = newDepositTokenMinted <> tfDeposit <> tfTotalDeposited
  logInfo $ "(submitContribution) will deposit in this pool: " <> show (tfOutRef, tfOut)
  -- We update the deposit state to include the new contribution
  datum <-
    updateDepositState PendingDepositState =<< addDeposit tfDeposits walletAddr

  feeConstraint <- getFeeConstraint
  let lookups =
        mconcat
          [ -- TF Minting Policy - for TF Token.
            Constraints.mintingPolicy tfTkMintingPolicy
          , -- Wallet Funds.
            Constraints.unspentOutputs walletFunds
          , -- Pool UTXO
            Constraints.unspentOutputs $ Map.singleton tfOutRef tfOut
          , -- TF Validator Script.
            Constraints.otherScript tfValidator
          ]

  let constraints =
        mconcat
          [ -- Spend TF Pool UTxO
            Constraints.mustSpendScriptOutput
              tfOutRef
              (wrapIntoRedeemer ())
          , -- Create new Pool UTxO
            Constraints.mustPayToOtherScript
              tfAddressHash
              datum
              tfValueLocked
          , -- Datum must be correct.
            Constraints.mustIncludeDatum datum
          , -- Minted/burnt TFTokens.
            Constraints.mustMintValueWithRedeemer (wrapIntoRedeemer Deposit) newDepositTokenMinted
          , feeConstraint
          ]

  case Constraints.mkTx @Void lookups constraints of
    Left err ->
      Contract.throwError $ ConstraintErr err
    Right x ->
      pure x
