module Spec.Tests.Initialise (
  initialisePoolTests,
) where

import Data.Functor (void)

import Data.Map.Strict qualified as Map

import Control.Monad.Reader (runReaderT)

import Test.Plutip.Contract qualified as PlutipContract

import Test.Plutip.Predicate qualified as PlutipPredicate
import Test.Tasty (TestTree)

import Plutus.Contract qualified as Contract

import Contract.Utils qualified as ContractUtils
import Test.Plutip.LocalCluster (withCluster)
import ThriftFinance.Common.Types (AppConfig)
import ThriftFinance.Offchain.Contracts.Initialise (initialisePool)
import ThriftFinance.Offchain.Error (TFError (Err))

import GHC.Base (NonEmpty ((:|)))
import Spec.Plutip.Common (TestRunner, initConfig)
import Test.Plutip.Contract (TestWallets)
import ThriftFinance.Offchain.Utils (waitSomeTime)

initialisePoolTests :: TestTree
initialisePoolTests =
  withCluster
    "trivial contract tests"
    [ PlutipContract.assertExecution
        "initialise the lending pool"
        (feeRecipient <> PlutipContract.initAda [1000, 20, 20])
        (initConfig >>= createLendingPool)
        [PlutipPredicate.shouldSucceed]
    ]

-- | Test initialising the pool.
createLendingPool :: AppConfig -> TestRunner String TFError ()
createLendingPool appConfig = PlutipContract.withContractAs 1 . const $ do
  (ownAddr :| _) <- Contract.ownAddresses
  utxos <- Contract.utxosAt ownAddr
  ContractUtils.guardCond Err $ Map.size utxos >= 1
  unbalancedTx <- flip runReaderT appConfig $ do
    initialisePool
  _txId <- Contract.submitUnbalancedTx unbalancedTx
  void waitSomeTime

feeRecipient :: TestWallets
feeRecipient = PlutipContract.initAda [555]
