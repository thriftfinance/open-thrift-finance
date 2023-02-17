{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spec.Tests.TrivialProtocol (trivialProtocolTests) where

import Scripts.V1.Deserialize (
  readConstTrueValidator,
  readOtherConstTrueValidator,
  readTrivialRewardTkF,
  readTrivialTkF,
 )

import ThriftFinance.Offchain.Contracts.TrivialProtocol (lockFunds, unlockFunds)

import Control.Monad.IO.Class (liftIO)

import Plutus.Contract.Error (ContractError)
import Plutus.V1.Ledger.Scripts (MintingPolicy)

import Spec.Plutip.Common (TestRunner, scriptCurrencySymbol)
import Test.Plutip.Contract qualified as PlutipContract

import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate qualified as PlutipPredicate

import Test.Tasty (TestTree)

trivialProtocolTests :: TestTree
trivialProtocolTests =
  withCluster
    "trivial contract tests"
    [ PlutipContract.assertExecution
        "locks funds / mints liquidity tokens correctly"
        initialWallet
        lockFundsCorrectly
        [PlutipPredicate.shouldSucceed]
    , PlutipContract.assertExecution
        "locks funds in different validator"
        initialWallet
        lockFundsOtherValidator
        [PlutipPredicate.shouldFail]
    , PlutipContract.assertExecution
        "locks funds, but minting too many liquidity tokens"
        initialWallet
        lockFundsButMintsTooMuch
        [PlutipPredicate.shouldFail]
    , PlutipContract.assertExecution
        "locks and unlocks funds correctly"
        initialWallet
        lockAndUnlockFunds
        [PlutipPredicate.shouldSucceed]
    , PlutipContract.assertExecution
        "locks and unlocks funds, but minting too many reward tokens"
        initialWallet
        lockAndUnlockFundsWithMoreRewards
        [PlutipPredicate.shouldFail]
    ]

-- | Locks funds in the correct validator and mints the right number of tokens
lockFundsCorrectly :: TestRunner String ContractError ()
lockFundsCorrectly = do
  -- Load validators/policies from files
  constTrueValidator <- liftIO readConstTrueValidator
  trivialTkF <- liftIO readTrivialTkF
  -- Create minting policy from locking script address
  let trivialTokenMp :: MintingPolicy = trivialTkF constTrueValidator
      lockedAmount = 200_000_000
  PlutipContract.withContract $ \_ -> lockFunds lockedAmount lockedAmount constTrueValidator trivialTokenMp

-- | Locks funds in *other* validator, triggering a validation error
lockFundsOtherValidator :: TestRunner String ContractError ()
lockFundsOtherValidator = do
  -- Load validators/policies from files
  constTrueValidator <- liftIO readConstTrueValidator
  otherConstTrueValidator <- liftIO readOtherConstTrueValidator
  trivialTkF <- liftIO readTrivialTkF
  -- Create minting policy from locking script address...
  let trivialTokenMp :: MintingPolicy = trivialTkF constTrueValidator
      lockedAmount = 200_000_000
  -- ... but try to lock in a different validator
  PlutipContract.withContract $ \_ -> lockFunds lockedAmount lockedAmount otherConstTrueValidator trivialTokenMp

-- | Locks funds in the correct validator but mints too many tokens
lockFundsButMintsTooMuch :: TestRunner String ContractError ()
lockFundsButMintsTooMuch = do
  -- Load validators/policies from files
  constTrueValidator <- liftIO readConstTrueValidator
  trivialTkF <- liftIO readTrivialTkF
  -- Create minting policy from locking script address...
  let trivialTokenMp :: MintingPolicy = trivialTkF constTrueValidator
      lockedAmount :: Integer = 200_000_000
  PlutipContract.withContract $ \_ -> lockFunds lockedAmount (lockedAmount * 2) constTrueValidator trivialTokenMp

-- | Lock and unlock funds in sequence correctly
lockAndUnlockFunds :: TestRunner String ContractError ()
lockAndUnlockFunds = do
  -- Load validators/policies from files
  constTrueValidator <- liftIO readConstTrueValidator
  trivialTkF <- liftIO readTrivialTkF
  trivialRewardTkF <- liftIO readTrivialRewardTkF
  -- Create minting policy from locking script address...
  let trivialTokenMp :: MintingPolicy = trivialTkF constTrueValidator
      trivialRewardTokenMp :: MintingPolicy = trivialRewardTkF $ scriptCurrencySymbol trivialTokenMp
      lockedAmount :: Integer = 200_000_000
  PlutipContract.withContract $ \_ -> do
    lockFunds lockedAmount lockedAmount constTrueValidator trivialTokenMp
    unlockFunds lockedAmount (lockedAmount `div` 5) constTrueValidator trivialTokenMp trivialRewardTokenMp

-- | Lock funds and try to mint more rewards than allowed when unlocking them
lockAndUnlockFundsWithMoreRewards :: TestRunner String ContractError ()
lockAndUnlockFundsWithMoreRewards = do
  -- Load validators/policies from files
  constTrueValidator <- liftIO readConstTrueValidator
  trivialTkF <- liftIO readTrivialTkF
  trivialRewardTkF <- liftIO readTrivialRewardTkF
  -- Create minting policy from locking script address...
  let trivialTokenMp :: MintingPolicy = trivialTkF constTrueValidator
      trivialRewardTokenMp :: MintingPolicy = trivialRewardTkF $ scriptCurrencySymbol trivialTokenMp
      lockedAmount :: Integer = 200_000_000
  PlutipContract.withContract $ \_ -> do
    lockFunds lockedAmount lockedAmount constTrueValidator trivialTokenMp
    unlockFunds lockedAmount (lockedAmount * 2) constTrueValidator trivialTokenMp trivialRewardTokenMp

{- | Simple TestWallets with two values, sufficient for locking some Ada
 | and setting a collateral
-}
initialWallet :: PlutipContract.TestWallets
initialWallet = PlutipContract.initAda [1000, 200]
