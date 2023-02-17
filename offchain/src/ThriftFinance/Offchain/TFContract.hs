{-# OPTIONS_GHC -Wno-unused-imports #-}

module ThriftFinance.Offchain.TFContract (
  type TFContract,
  getTfValidator,
  getTfValidatorAddress,
  getTpValidator,
  getTpPolicies,
  getTfTokenCs,
  getTPInfo,
  getTfTkMp,
  checksEnabled,
  logDebug,
  logInfo,
) where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Map qualified as Map
import Plutus.Contract (Contract, throwError)
import Plutus.Contract qualified as Contract

import Ledger (ChainIndexTxOut (ScriptChainIndexTxOut), _ciTxOutDatum, _ciTxOutValue)
import Ledger.Tx (ChainIndexTxOut, TxOutRef)
import Ledger.Value (assetClass)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Api (Address, MintingPolicy, Validator, Value, adaSymbol, adaToken, fromBuiltinData, getDatum)
import Plutus.V1.Ledger.Value (assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (CurrencySymbol)
import ThriftFinance.Common.Types (
  AppConfig (),
  DepositState (ActiveDepositState, PendingDepositState),
  TFDatum,
  TargetProtocolInfo (tpi'DepositTarget, tpi'NumberOfContributions),
  tf'TokenMp,
  tf'Validator,
  tf'offchainChecksEnabled,
  tf'targetProtocolInfo,
  tf'targetProtocolMintingPolicies,
  tf'targetProtocolValidators,
 )
import ThriftFinance.Offchain.Error (OtherError, TFError (OtherErr))

{- | The type of the ThriftFinance Contract - we'll use a ReaderT here to make
 it easier to compose the contracts.
-}
type TFContract a = forall w s. ReaderT AppConfig (Contract w s TFError) a

-- | Returns the TF Validator.
getTfValidator :: TFContract Validator
getTfValidator = asks tf'Validator

-- | Returns the TF validator's address.
getTfValidatorAddress :: TFContract Address
getTfValidatorAddress = mkValidatorAddress <$> getTfValidator

-- | Returns the target protocol's validator
getTpValidator :: TFContract Validator
getTpValidator = asks tf'targetProtocolValidators

-- | Returns the target protocol's policies
getTpPolicies :: TFContract [MintingPolicy]
getTpPolicies = asks tf'targetProtocolMintingPolicies

-- | Returns the TF Tk CurrencySymbol.
getTfTokenCs :: TFContract CurrencySymbol
getTfTokenCs = asks (scriptCurrencySymbol . tf'TokenMp)

getTPInfo :: TFContract TargetProtocolInfo
getTPInfo = asks tf'targetProtocolInfo

getTfTkMp :: TFContract MintingPolicy
getTfTkMp = asks tf'TokenMp

-- | Returns true if offchain checks are enabled in AppConfig
checksEnabled :: TFContract Bool
checksEnabled = asks tf'offchainChecksEnabled

logInfo :: String -> TFContract ()
logInfo = lift . Contract.logInfo

logDebug :: String -> TFContract ()
logDebug = lift . Contract.logDebug
