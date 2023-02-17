module ThriftFinance.Offchain.Contracts.Return (
  returnPool,
) where

import ThriftFinance.Offchain.Error (
  OtherError (CouldNotGetTargetPolicies),
  ReturnPoolError (CouldNotSelectWinner),
  TFError (ConstraintErr, OtherErr, ReturnPoolErr),
 )

import ThriftFinance.Offchain.TFContract (
  TFContract,
  getTPInfo,
  getTfTkMp,
  getTfTokenCs,
  getTfValidator,
  getTpPolicies,
  getTpValidator,
  logInfo,
 )

import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Void (Void)

import Ledger (getPubKeyHash, scriptCurrencySymbol)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (UnbalancedTx)
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (throwError)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))

import ThriftFinance.Common.Types (
  DepositState (ActiveDepositState),
  Deposits (Deposits),
  TFRedeemer (Return),
  TrivialRewardTkMpRedeemer (MintRewardTk),
  TrivialTkMpRedeemer (MintTk),
  toList,
  tpi'DepositTarget,
  tpi'NumberOfContributions,
  unDeposits,
  unOrdMap,
 )

import ThriftFinance.Common.Constants (
  tfActiveTokenName,
  trivialRewardTokenName,
  trivialTokenName,
 )

import ThriftFinance.Offchain.Utils (
  getCredsFromEntry,
  getFeeConstraint,
  getFirstOwnWalletAddress,
  getReturnValue,
  getTfActiveUtxos,
  getTpUtxos,
  payToConstraint,
  updateDepositState,
  wrapIntoRedeemer,
 )

import Plutus.Contract.Request qualified as Contract

import Plutus.V1.Ledger.Api (CurrencySymbol, PubKeyHash, StakingCredential, TokenName)

-- Trigger the return transaction.
returnPool :: TFContract UnbalancedTx
returnPool = do
  logInfo "returnPool"
  walletAddr <- getFirstOwnWalletAddress
  walletFunds <- lift $ Contract.utxosAt walletAddr
  -- Retrieve TF information
  tfTokenCs <- getTfTokenCs
  tfTkMintingPolicy <- getTfTkMp
  tfValidator <- getTfValidator
  -- Retrieve target protocol's validator and minting policies
  tpInfo <- getTPInfo
  tpValidator <- getTpValidator
  (tpTkMintingPolicy, tpTkRewardMintingPolicy) <-
    getTpPolicies >>= \case
      [tok, rewardTok] -> pure (tok, rewardTok)
      _ -> throwError $ OtherErr CouldNotGetTargetPolicies
  let tpCs, tpRewardCs :: CurrencySymbol
      tpCs = scriptCurrencySymbol tpTkMintingPolicy
      tpRewardCs = scriptCurrencySymbol tpTkRewardMintingPolicy
      tpTokenName, tpRewardTokenName :: TokenName
      tpTokenName = trivialTokenName
      tpRewardTokenName = trivialRewardTokenName
  -- Get pool's UTXO, total value and active deposit information
  (tfOutRef, tfOut, deposits) :| _ <- getTfActiveUtxos
  -- Get Trivial Protocol's output
  (tpOutRef, tpOut) :| _ <- getTpUtxos
  -- Get the contributors' credentials from their addresses
  contributorCreds <-
    traverse getCredsFromEntry
      . toList
      . unDeposits
      $ deposits
  -- We mint an active token and burn all deposit tokens
  let adaLockedAmount = tpi'DepositTarget tpInfo
      -- Burn the active TF token
      newActiveTokenBurnt = assetClassValue (assetClass tfTokenCs tfActiveTokenName) (-1)
      -- Burn the liquidity tokens
      newTrivialTokensBurnt = assetClassValue (assetClass tpCs tpTokenName) (-adaLockedAmount)
      -- Mint 5:1 Trivial reward tokens for every Ada locked
      -- (and therefore every liquidity token minted)
      newTrivialRewardTokensMinted = assetClassValue (assetClass tpRewardCs tpRewardTokenName) (adaLockedAmount `div` 5)
  -- We don't update the deposit state this time - the transition is
  -- solely repersented by the ReturnToken
  datum <- updateDepositState ActiveDepositState deposits
  -- We pay the fees to the recipient
  feeConstraint <- getFeeConstraint
  -- We obtain the winner of the rewards
  winner <- getWinner deposits
  -- FIXME: Use proper min. ada calculation
  winnerConstraint <-
    payToConstraint winner $
      newTrivialRewardTokensMinted <> Ada.adaValueOf 5

  let lookups =
        mconcat
          [ -- TF Minting Policy - for TF Token.
            Constraints.mintingPolicy tfTkMintingPolicy
          , -- TP Minting Policy - for liquidity token
            Constraints.mintingPolicy tpTkMintingPolicy
          , -- TP Minting Policy - for reward token
            Constraints.mintingPolicy tpTkRewardMintingPolicy
          , -- Wallet Funds.
            Constraints.unspentOutputs walletFunds
          , -- TF Pool utxo
            Constraints.unspentOutputs $ Map.singleton tfOutRef tfOut
          , -- Target protocol utxo
            Constraints.unspentOutputs $ Map.singleton tpOutRef tpOut
          , -- TF Validator Script.
            Constraints.otherScript tfValidator
          , -- TP Validator script
            Constraints.otherScript tpValidator
          ]

  let contributorsReturn =
        foldMap
          ( \(pkh, (spkh', n)) ->
              maybe
                -- If no StakeKeyHash is available, just use `mustPayToPubKey`
                (Constraints.mustPayToPubKey pkh $ getReturnValue tpInfo n)
                -- If a StakeKeyHash is available, use it with `mustPayToPubKeyAddress`
                (\spkh -> Constraints.mustPayToPubKeyAddress pkh spkh $ getReturnValue tpInfo n)
                spkh'
          )
          contributorCreds

  let constraints =
        mconcat
          [ -- We spend the TF pool's output
            Constraints.mustSpendScriptOutput
              tfOutRef
              (wrapIntoRedeemer ())
          , -- We spend the TP's output
            Constraints.mustSpendScriptOutput
              tpOutRef
              (wrapIntoRedeemer ())
          , -- We return the contributions to their users
            contributorsReturn
          , -- We send the rewards to the winner
            winnerConstraint
          , -- Datum must be correct.
            Constraints.mustIncludeDatum datum
          , -- Minted/burnt TFTokens.
            Constraints.mustMintValueWithRedeemer (wrapIntoRedeemer Return) newActiveTokenBurnt
          , -- Burnt Trivial protocol's tokens
            Constraints.mustMintValueWithRedeemer (wrapIntoRedeemer MintTk) newTrivialTokensBurnt
          , -- Minted Trivial protocol's tokens
            Constraints.mustMintValueWithRedeemer (wrapIntoRedeemer MintRewardTk) newTrivialRewardTokensMinted
          , feeConstraint
          ]
  case Constraints.mkTx @Void lookups constraints of
    Left e ->
      throwError $ ConstraintErr e
    Right x ->
      pure x

-- | Use the participants' key hashes to obtain a winner
getWinner :: Deposits -> TFContract (PubKeyHash, (Maybe StakingCredential, Integer))
getWinner (Deposits (unOrdMap -> contribMap)) = do
  tpInfo <- getTPInfo
  let n :: Int
      n = fromIntegral $ sum (snd <$> AssocMap.elems contribMap)
      candidates :: [(PubKeyHash, (Maybe StakingCredential, Integer))]
      candidates = zip (AssocMap.keys contribMap) (AssocMap.elems contribMap)
  if fromInteger (tpi'NumberOfContributions tpInfo) /= n
    then throwError $ ReturnPoolErr CouldNotSelectWinner
    else pure $ candidates !! (hashContributions (fst <$> candidates) `mod` n)
  where
    hashContributions :: [PubKeyHash] -> Int
    hashContributions = foldl (\acc addr -> acc + toNum addr) 0
    toNum :: PubKeyHash -> Int
    toNum = fromIntegral . BS.foldl (+) 0 . toBs . getPubKeyHash
    toBs (BuiltinByteString bs) = bs
