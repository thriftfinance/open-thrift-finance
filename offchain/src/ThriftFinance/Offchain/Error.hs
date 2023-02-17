module ThriftFinance.Offchain.Error (
  TFError (..),
  TriggerPoolError (..),
  SubmitPoolError (..),
  ReturnPoolError (..),
  OtherError (..),
) where

import Control.Lens (prism')
import Ledger.Constraints (MkTxError)
import Ledger.Value (Value)
import Plutus.Contract (AsContractError (_ContractError), ContractError)

-- | Types of offchain errors. Extend data-type as needed.
data TFError
  = -- | Errors related to initialising a pool
    InitialisePoolErr ContractError
  | -- | Errors related to triggering a pending deposit
    TriggerPoolErr TriggerPoolError
  | -- | Errors related to making a contribution to the pool
    SubmitPoolErr SubmitPoolError
  | -- | Error related to returning the deposits and giving out rewards
    ReturnPoolErr ReturnPoolError
  | -- | General errors, not specific to any action in particular
    OtherErr OtherError
  | -- | Contract errors
    ContractErr ContractError
  | -- | Constraint errors
    ConstraintErr MkTxError
  | -- | Unspecified error
    Err
  deriving stock (Show)

data SubmitPoolError
  = NoPoolsToContribute
  deriving stock (Show)

data TriggerPoolError
  = BadNumberOfContributions {expectedNumber :: Integer, number :: Integer}
  | BadAmountOfFunds {expectedValue :: Value, value :: Value}
  deriving stock (Show)

data ReturnPoolError
  = CouldNotSelectWinner
  deriving stock (Show)

data OtherError
  = CouldNotCreateValidatorHash
  | CouldNotGetPoolUtxo
  | CouldNotGetTargetPortfolio
  | CouldNotGetPendingDepositInfo
  | CouldNotGetActiveDepositInfo
  | CouldNotGetAmountDeposited
  | CouldNotGetTargetProtocolOutput
  | CouldNotGetContributorCredentials
  | CouldNotGetTargetPolicies
  deriving stock (Show)

instance AsContractError TFError where
  _ContractError = prism' ContractErr $ \case
    InitialisePoolErr e -> Just e
    ContractErr e -> Just e
    _ -> Nothing
