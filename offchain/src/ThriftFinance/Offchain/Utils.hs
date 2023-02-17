{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module ThriftFinance.Offchain.Utils (
  wrapIntoDatum,
  getTime,
  wrapIntoRedeemer,
  updateDepositState,
  valueOf',
  chainTxValue,
  validatorAddress,
  validatorHashBs,
  getValidatorAdressHash,
  getFirstOwnWalletAddress,
  getTfUtxos,
  getTpUtxos,
  getDepositValue,
  getReturnValue,
  getTfTargetPortfolios,
  getTfActiveUtxos,
  getTfPendingUtxos,
  getAmountDeposited,
  getTfTargetProtocolInfo,
  getFeeConstraint,
  payToConstraint,
  waitSomeTime,
  addDeposit,
  newDepositEntry,
  getCredsFromEntry,
) where

import Control.Monad ((<=<))
import Control.Monad.Trans (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Ledger (Address (..), ChainIndexTxOut (..), POSIXTime, PubKeyHash, TxOut (txOutValue), TxOutRef, getDatum, scriptHashAddress, toTxOut, validatorHash)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract.Request qualified as Contract
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  BuiltinData (BuiltinData),
  Credential (..),
  CurrencySymbol,
  Datum (Datum),
  Redeemer (Redeemer),
  StakingCredential (..),
  TokenName,
  Validator,
  ValidatorHash (ValidatorHash),
  Value,
  fromBuiltinData,
 )

import GHC.Base (NonEmpty ((:|)))

import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Address (toValidatorHash)

import Control.Monad.RWS (MonadReader (ask))
import Ledger.Address (PaymentPubKeyHash (..), StakePubKeyHash (..))
import Ledger.Constraints (TxConstraints)
import Ledger.Slot (Slot)
import Plutus.Contract (AsContractError, Contract)
import ThriftFinance.Common.Types (
  DepositState (..),
  Deposits (..),
  TFDatum (..),
  TargetProtocolInfo (..),
  fromMap,
  tf'targetProtocolInfo,
  tpi'DepositTarget,
  tpi'NumberOfContributions,
  unOrdMap,
 )
import ThriftFinance.Offchain.Error (OtherError (..), TFError (OtherErr))
import ThriftFinance.Offchain.TFContract (TFContract, getTPInfo, getTfValidatorAddress, getTpValidator)

-- | Helper for creating 'Datum's from data-encode-able types.
wrapIntoDatum :: PlutusTx.ToData a => a -> Datum
wrapIntoDatum = Datum . BuiltinData . PlutusTx.toData

-- | Helper for creating 'Redeemer's from data-encode-able types.
wrapIntoRedeemer :: PlutusTx.ToData a => a -> Redeemer
wrapIntoRedeemer = Redeemer . BuiltinData . PlutusTx.toData

{- | Helper for updating the deposit state of a TFDatum. It uses the
 the reader context to automatically fill in the target protocol info.
-}
updateDepositState :: DepositState -> Deposits -> TFContract Datum
updateDepositState newState deposits = do
  tpInfo <- getTPInfo
  pure $ wrapIntoDatum $ TFDatum tpInfo newState deposits

-- | 'Ledger.valueOf' with actually sane argument ordering.
valueOf' :: CurrencySymbol -> TokenName -> Value -> Integer
valueOf' c tk v = Value.valueOf v c tk

chainTxValue :: ChainIndexTxOut -> Value
chainTxValue = txOutValue . toTxOut

-- | Gets the address of a validator
validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash

-- | Get a validator's ScriptHash as a BuiltinByteString
validatorHashBs :: Validator -> BuiltinByteString
validatorHashBs vl = let (ValidatorHash bs) = validatorHash vl in bs

-- | Get a Validator's Hash.
getValidatorAdressHash :: Address -> TFContract ValidatorHash
getValidatorAdressHash adr =
  case toValidatorHash adr of
    Just x -> pure x
    Nothing -> Contract.throwError $ OtherErr CouldNotCreateValidatorHash

-- | Get first address of own wallet.
getFirstOwnWalletAddress :: TFContract Address
getFirstOwnWalletAddress = do
  walletAddr :| _ <- lift Contract.ownAddresses
  return walletAddr

-- | Returns the correct Value for a single deposit.
getDepositValue :: TFContract Value
getDepositValue = do
  tpInfo <- getTPInfo
  let targetDeposit = tpi'DepositTarget tpInfo
      nContributions = tpi'NumberOfContributions tpInfo
      targetCs = tpi'PassiveDepositCurrency tpInfo
      targetTn = tpi'PassiveDepositName tpInfo
  pure $ Value.singleton targetCs targetTn (targetDeposit `div` nContributions)

-- | Returns the correct Value to return based on the number of contributions
getReturnValue :: TargetProtocolInfo -> Integer -> Value
getReturnValue tpInfo n =
  let targetValue = tpi'DepositTarget tpInfo
      targetContributions = tpi'NumberOfContributions tpInfo
      targetCs = tpi'PassiveDepositCurrency tpInfo
      targetTn = tpi'PassiveDepositName tpInfo
   in Value.singleton targetCs targetTn (n * targetValue `div` targetContributions)

-- | Returns pool UTXOs locked by the validator.
getTfUtxos :: TFContract (NonEmpty (TxOutRef, ChainIndexTxOut))
getTfUtxos = do
  validatorAddr <- getTfValidatorAddress
  utxos <- lift $ Contract.utxosAt validatorAddr
  case Map.toList utxos of
    [] -> Contract.throwError $ OtherErr CouldNotGetPoolUtxo
    (x : xs) -> pure $ x :| xs

-- | Return specific pool UTXOs for this contract
getTfTargetPortfolios :: TFContract (NonEmpty (TxOutRef, ChainIndexTxOut, TFDatum))
getTfTargetPortfolios = do
  tpInfo <- getTfTargetProtocolInfo
  targetPortfolios <-
    fmap (sequence . filter isJust) $
      traverse (f tpInfo) . NonEmpty.toList
        =<< getTfUtxos
  case targetPortfolios of
    Just (x : xs) -> pure $ x :| xs
    _ -> Contract.throwError $ OtherErr CouldNotGetTargetPortfolio
  where
    -- Keeps UTXOs that match the target protocol info and adds the datum
    f :: TargetProtocolInfo -> (TxOutRef, ChainIndexTxOut) -> TFContract (Maybe (TxOutRef, ChainIndexTxOut, TFDatum))
    f tpInfo (txOutRef, txOut@(ScriptChainIndexTxOut _ _ outDatum _)) =
      case outDatum of
        Left datHash -> do
          dat' <- lift $ (fromBuiltinData <=< fmap getDatum) <$> Contract.datumFromHash datHash
          case dat' of
            Just dat ->
              if isTargetProtocol tpInfo dat
                then pure $ Just (txOutRef, txOut, dat)
                else pure Nothing
            _ -> pure Nothing
        Right (fromBuiltinData . getDatum -> Just dat) -> do
          if isTargetProtocol tpInfo dat
            then pure $ Just (txOutRef, txOut, dat)
            else pure Nothing
        _ -> do
          pure Nothing
    f _ _ = do
      pure Nothing
    isTargetProtocol :: TargetProtocolInfo -> TFDatum -> Bool
    isTargetProtocol tpInfo (TFDatum tpInfo' _ _) = tpInfo == tpInfo'

-- | Return specific pool UTXOs for this contract that are in pending state
getTfPendingUtxos :: TFContract (NonEmpty (TxOutRef, ChainIndexTxOut, Deposits))
getTfPendingUtxos = do
  utxos <- mapMaybe f . NonEmpty.toList <$> getTfTargetPortfolios
  case utxos of
    [] -> Contract.throwError $ OtherErr CouldNotGetPendingDepositInfo
    (x : xs) -> pure $ x :| xs
  where
    f (ref, out, TFDatum _ PendingDepositState deposits) = Just (ref, out, deposits)
    f _ = Nothing

-- | Return specific pool UTXOs for this contract that are in active state
getTfActiveUtxos :: TFContract (NonEmpty (TxOutRef, ChainIndexTxOut, Deposits))
getTfActiveUtxos = do
  utxos <- mapMaybe f . NonEmpty.toList <$> getTfTargetPortfolios
  case utxos of
    [] -> Contract.throwError $ OtherErr CouldNotGetActiveDepositInfo
    (x : xs) -> pure $ x :| xs
  where
    f (ref, out, TFDatum _ ActiveDepositState deposits) = Just (ref, out, deposits)
    f _ = Nothing

{- | Return specific TrivialProtocol's UTxOs that have enough funds to be used
 in the Return action
-}
getTpUtxos :: TFContract (NonEmpty (TxOutRef, ChainIndexTxOut))
getTpUtxos = do
  tpInfo <- getTPInfo
  validatorAddr <- validatorAddress <$> getTpValidator
  utxos <- lift $ Contract.utxosAt validatorAddr
  case Map.toList $ Map.filter (f tpInfo) utxos of
    [] -> Contract.throwError $ OtherErr CouldNotGetTargetProtocolOutput
    (x : xs) -> pure $ x :| xs
  where
    f tpInfo txOut =
      let targetCs = tpi'PassiveDepositCurrency tpInfo
          targetTn = tpi'PassiveDepositName tpInfo
          target = tpi'DepositTarget tpInfo
          count = valueOf' targetCs targetTn
       in count (_ciTxOutValue txOut) == target

-- | Return the total amount of target currency deposited in the pool
getAmountDeposited :: ChainIndexTxOut -> TFContract Value
getAmountDeposited txOut =
  case txOut of
    ScriptChainIndexTxOut {_ciTxOutValue = v} -> pure v
    _ -> Contract.throwError $ OtherErr CouldNotGetAmountDeposited

-- | FIXME: We get the approximate time by querying the tip of the chain
getTime :: TFContract POSIXTime
getTime = do
  _tip <- lift Contract.getTip
  undefined

getTfTargetProtocolInfo :: TFContract TargetProtocolInfo
getTfTargetProtocolInfo = tf'targetProtocolInfo <$> ask

-- | Constraints for paying the fees to the correct recipient
getFeeConstraint :: forall a b. TFContract (TxConstraints a b)
getFeeConstraint = do
  tpInfo <- getTPInfo
  let fee = tpi'Fee tpInfo
      recipient = tpi'FeeRecipient tpInfo
  pkh <- getPubKeyHash recipient
  payToConstraint (pkh, (Nothing, 1)) fee

-- | Convenient wrapper for paying to a user with a given `Address`
payToConstraint :: forall a b. (PubKeyHash, (Maybe StakingCredential, Integer)) -> Value -> TFContract (TxConstraints a b)
payToConstraint entry val = do
  (pkh, (spkh, _)) <- getCredsFromEntry entry
  pure $
    maybe
      (Constraints.mustPayToPubKey pkh val)
      (\spkh' -> Constraints.mustPayToPubKeyAddress pkh spkh' val)
      spkh

-- | Poor man's substitute for `awaitTxConfirmed`.
waitSomeTime :: forall w s e. AsContractError e => Contract w s e Slot
waitSomeTime = Contract.waitNSlots 8

{- | Add/modify deposit. If no deposit has been made before, a new entry is
 inserted. Otherwise, the existing entry is modified.
-}
addDeposit :: Deposits -> Address -> TFContract Deposits
addDeposit (Deposits (unOrdMap -> m)) addr = do
  pkh <- getPubKeyHash addr
  case AssocMap.lookup pkh m of
    -- If present, increase contributions by 1
    Just (stakeCred, n) ->
      pure . Deposits . fromMap . AssocMap.insert pkh (stakeCred, n + 1) $ AssocMap.delete pkh m
    -- If not present, create a new entry
    Nothing -> do
      (_, v) <- newDepositEntry addr
      pure . Deposits . fromMap $ AssocMap.insert pkh v m

-- | Generate a new deposit entry from an Address.
newDepositEntry :: Address -> TFContract (PubKeyHash, (Maybe StakingCredential, Integer))
newDepositEntry addr = do
  pkh <- getPubKeyHash addr
  pure (pkh, (addressStakingCredential addr, 1))

{- | Retrieves the PubKeyHash from a PubKeyCredential. If the address is has
 a ScriptCredential, it fails.
-}
getPubKeyHash :: Address -> TFContract PubKeyHash
getPubKeyHash Address {addressCredential} =
  case addressCredential of
    PubKeyCredential pkh -> pure pkh
    _ -> Contract.throwError $ OtherErr CouldNotGetContributorCredentials

-- | Retrieves the PaymentPubKeyHash and StakePubKeyHash from a contribution entry
getCredsFromEntry :: (PubKeyHash, (Maybe StakingCredential, Integer)) -> TFContract (PaymentPubKeyHash, (Maybe StakePubKeyHash, Integer))
getCredsFromEntry (pkh, (skh', n)) = do
  skh <- traverse getSkh skh'
  pure (PaymentPubKeyHash pkh, (skh, n))
  where
    getSkh :: StakingCredential -> TFContract StakePubKeyHash
    getSkh = \case
      StakingHash (PubKeyCredential pkh) -> pure $ StakePubKeyHash pkh
      _ -> Contract.throwError $ OtherErr CouldNotGetContributorCredentials
