{-# LANGUAGE CPP #-}

module ThriftFinance.Common.Types
  ( TargetProtocolInfo (..)
  , DepositState (..)
  , Deposits (..)
  , TFDatum (..)
  , TFRedeemer(..)
  , AppConfig (..)
  , TrivialTkMpRedeemer (..)
  , TrivialRewardTkMpRedeemer (..)
  , OrdMap
  , fromList
  , fromMap
  , toList
  , unOrdMap
  ) where

import PlutusTx.AssocMap(Map)
import qualified PlutusTx.AssocMap as AssocMap

#ifdef NEW_LEDGER_NAMESPACE

import PlutusLedgerApi.V1(
  Address,
  Value,
  CurrencySymbol,
  TokenName,
  Validator,
  MintingPolicy,
  PubKeyHash,
  StakingCredential,
  FromData,
  ToData,
  UnsafeFromData
  )

#else

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Api (Value, CurrencySymbol, MintingPolicy, Validator, TokenName, PubKeyHash, StakingCredential, FromData, ToData, UnsafeFromData)

#endif

import GHC.Generics (Generic)

import qualified PlutusTx
import Data.List (sortOn)

-- | The configuration of the Thrift Finance App.
data AppConfig = AppConfig
  { -- | Thrifth Finance Minting Policy.
    tf'TokenMp   :: MintingPolicy
  , -- |  Thrifth Finance Validator.
    tf'Validator :: Validator
  , -- | The target protocol Information
    tf'targetProtocolInfo :: TargetProtocolInfo
  , -- | Target Protocol Validator - used in the protocol.
    tf'targetProtocolValidators :: Validator
  , -- | Target Protocol Minting Policies - used in the protocol.
    tf'targetProtocolMintingPolicies :: [ MintingPolicy ]
  , -- | This flag allows disabling offchain validation checks, which is
    -- | useful for testing onchain code
    tf'offchainChecksEnabled :: Bool
  }
  deriving stock (Show,Eq,Generic)

-- | The target protocol information contains all the information regarding to
-- what will happen with the submitted information.
data TargetProtocolInfo = TargetProtocolInfo
  { tpi'LockingScript :: Address
    -- ^ The locking script is indicative of where the funds will be locked
    -- after the execution of th

  , tpi'DepositTarget :: Integer
    -- ^ Deposit target is the total amount of funds expected to be raised
    -- before the deposit is triggered.

  , tpi'NumberOfContributions :: Integer
    -- ^ Deposit indicates how many contributions need to be made to reach the
    -- target. It should be chosen such that the deposit target is divisible by
    -- the amount of expected Number of Contributions.

  , tpi'PassiveDepositCurrency :: CurrencySymbol
    -- ^ The expected currency symbol of the deposits

  , tpi'PassiveDepositName :: TokenName
    -- ^ The expected currency name of the deposits

  , tpi'ActiveDepositCurrency :: CurrencySymbol
    -- ^ The expected currency symbol of the return of the depositing action.

  , tpi'ClaimReturnAfter :: Integer
    -- ^ Indicates how long after the triggering of the deposit the Return
    -- transaction can be triggered.

  , tpi'Fee :: Value
    -- ^ Flat fee paid by all contributors

  , tpi'FeeRecipient :: Address
    -- ^ Address of the recipient of all the fees paid throughout the protocol
  }
  deriving stock (Show,Eq,Generic)

-- | Thrift finance EUTxO Datum types - it contains the target protocol info
-- and the current state of the deposits
data TFDatum = TFDatum TargetProtocolInfo DepositState Deposits
  deriving stock (Show,Eq,Generic)

-- | The deposits can either be pending or active
data DepositState
  = PendingDepositState
  | ActiveDepositState
  deriving stock (Show,Eq,Generic)

-- | The deposits - a map from each user's PKH to her (optional) staking
-- credential and the number of contributions
newtype Deposits = Deposits { unDeposits :: OrdMap PubKeyHash (Maybe StakingCredential, Integer)}
  deriving stock (Show, Generic)
  deriving newtype (Eq, UnsafeFromData, FromData, ToData)

-- | Thrift finance redeemers - describe the step functions available for the
-- Thrift Finance protocol.
data TFRedeemer
  = Initialise
  | Deposit
  | Activate
  | Return
  deriving stock (Show,Eq,Generic)

-- | Trivial Token Minting Policy Redeemer.
data TrivialTkMpRedeemer = MintTk
  deriving stock (Show,Eq,Generic)
 
-- | Trivial Reward Token Minting Policy Redeemer.
data TrivialRewardTkMpRedeemer = MintRewardTk
  deriving stock (Show,Eq,Generic)

-- | A wrapper over `PlutusTx.Map` with sortedness guarantees
newtype OrdMap k v = OrdMap {unOrdMap :: Map k v}
  deriving stock (Show, Generic)
  deriving newtype (Eq, UnsafeFromData, FromData, ToData)

-- | Smart constructor for `OrdMap`.
fromList :: forall k v . Ord k => [(k, v)] -> OrdMap k v
fromList = OrdMap . AssocMap.fromList . sortOn fst

fromMap :: forall k v . Ord k => Map k v -> OrdMap k v
fromMap = fromList . AssocMap.toList

toList :: OrdMap k v -> [(k, v)]
toList = AssocMap.toList . unOrdMap

PlutusTx.unstableMakeIsData ''TargetProtocolInfo
PlutusTx.unstableMakeIsData ''DepositState
PlutusTx.unstableMakeIsData ''TFDatum
PlutusTx.unstableMakeIsData ''TFRedeemer
PlutusTx.unstableMakeIsData ''TrivialTkMpRedeemer
PlutusTx.unstableMakeIsData ''TrivialRewardTkMpRedeemer
