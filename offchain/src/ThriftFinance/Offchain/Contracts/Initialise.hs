module ThriftFinance.Offchain.Contracts.Initialise (
  initialisePool,
) where

import ThriftFinance.Offchain.Error (TFError (ConstraintErr))
import ThriftFinance.Offchain.TFContract (
  TFContract,
  getTPInfo,
  getTfTkMp,
  getTfTokenCs,
  getTfValidator,
  getTfValidatorAddress,
  logInfo,
 )
import ThriftFinance.Offchain.Utils (
  getDepositValue,
  getFeeConstraint,
  getFirstOwnWalletAddress,
  getValidatorAdressHash,
  newDepositEntry,
  wrapIntoDatum,
  wrapIntoRedeemer,
 )

import ThriftFinance.Common.Types (
  DepositState (PendingDepositState),
  Deposits (Deposits),
  TFDatum (TFDatum),
  TFRedeemer (Initialise),
  fromList,
 )

import Control.Monad.Trans (lift)
import Data.Void (Void)

import Ledger.Constraints (UnbalancedTx)
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (throwError)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)

import ThriftFinance.Common.Constants (tfDepositTokenName)

{- | Initialise a Thrift Finance peer lending pool - Create an EUTxO with the
 correct information.
-}
initialisePool :: TFContract UnbalancedTx
initialisePool = do
  logInfo "initialisePool"
  tfTokenCs <- getTfTokenCs
  tfTkMintingPolicy <- getTfTkMp
  tfAddress <- getTfValidatorAddress
  tfValidator <- getTfValidator
  tfAddressHash <- getValidatorAdressHash tfAddress
  tpInfo <- getTPInfo
  depositValue <- getDepositValue
  lift $ Contract.logDebug @String "Printing deposit value"
  lift $ Contract.logDebug depositValue
  walletAddr <- getFirstOwnWalletAddress
  walletFunds <- lift $ Contract.utxosAt walletAddr
  feeConstraint <- getFeeConstraint
  depositEntry <- newDepositEntry walletAddr
  let datum = wrapIntoDatum $ TFDatum tpInfo PendingDepositState $ Deposits (fromList [depositEntry])

  let mintedValue = assetClassValue (assetClass tfTokenCs tfDepositTokenName) 1

  let lookups =
        mconcat
          [ -- TF Minting Policy - for TF Token.
            Constraints.mintingPolicy tfTkMintingPolicy
          , -- Wallet Funds.
            Constraints.unspentOutputs walletFunds
          , -- TF Validator Script.
            Constraints.otherScript tfValidator
          ]

  let constraints =
        mconcat
          [ -- Datum must be correct.
            Constraints.mustIncludeDatum datum
          , -- Minted TFToken.
            Constraints.mustMintValueWithRedeemer (wrapIntoRedeemer Initialise) mintedValue
          , -- Funds that must be Paid to the TFAddress.
            Constraints.mustPayToOtherScript
              tfAddressHash
              datum
              ( mconcat
                  [ mintedValue -- The TFToken
                  , depositValue -- Deposit Value
                  ]
              )
          , feeConstraint
          ]
  case Constraints.mkTx @Void lookups constraints of
    Left e ->
      throwError $ ConstraintErr e
    Right x ->
      pure x
