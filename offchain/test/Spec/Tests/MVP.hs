module Spec.Tests.MVP (
  mvpTests,
) where

import Spec.Plutip.Common (TestRunner, TestRunnerM, initConfig, runTFContract)

import Test.Plutip.Contract (TestWallets)
import Test.Plutip.Contract qualified as PlutipContract

import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate qualified as PlutipPredicate

import Control.Monad.Reader (lift)
import Ledger (Slot)
import Ledger.Constraints (UnbalancedTx)
import Plutus.Contract qualified as Contract
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace))
import Test.Tasty (TestTree)
import ThriftFinance.Common.Types (AppConfig)
import ThriftFinance.Offchain.Contracts.Initialise (initialisePool)
import ThriftFinance.Offchain.Contracts.Submit (submitContribution)
import ThriftFinance.Offchain.Contracts.Trigger (triggerPool)
import ThriftFinance.Offchain.Error (TFError)
import ThriftFinance.Offchain.TFContract (TFContract)
import ThriftFinance.Offchain.Utils (waitSomeTime)

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty)
import Ledger.Value (Value)
import Test.Plutip.Internal.Types (ExecutionResult (outcome))
import ThriftFinance.Offchain.Contracts.Return (returnPool)

mvpTests :: TestTree
mvpTests =
  withCluster
    "trivial contract tests"
    [ PlutipContract.assertExecutionWith
        [ShowTrace, ShowBudgets]
        "MVP Demo"
        initialWallets
        (initConfig >>= mvp)
        [PlutipPredicate.shouldSucceed]
    ]

-- | The whole demo trace
mvp :: AppConfig -> TestRunner String TFError Slot
mvp cfg = do
  -- Ana initialises the pool with a target of 1000 Ada and makes
  -- a deposit of 250 Ada
  _ <- withContractAs' cfg anaId initialisePool
  -- Ben makes a deposit of 250 Ada
  _ <- withContractAs' cfg benId submitContribution
  -- Charlie makes two deposits of 250 Ada, reaching the target
  _ <- withContractAs' cfg charlieId submitContribution
  _ <- withContractAs' cfg charlieId submitContribution
  -- Ben activates the deposit
  _ <- withContractAs' cfg benId triggerPool
  -- Charlie collects the rewards
  withContractAs' cfg charlieId returnPool

initialWallets :: TestWallets
initialWallets =
  foldl1 (<>) $
    PlutipContract.initAda
      <$> [ [515] -- Fee recipient wallet
          , replicate 5 250 -- Ana
          , replicate 5 250 -- Ben
          , replicate 5 250 -- Charlie
          ]

anaId, benId, charlieId :: Int
(anaId, benId, charlieId) = (1, 2, 3)

submitAndWait :: UnbalancedTx -> TFContract Slot
submitAndWait unbalancedTx = do
  _txId <- lift $ Contract.submitUnbalancedTx unbalancedTx
  lift waitSomeTime

{- | Helper for running any contract and waiting some slots for the transaction
 to be processed
-}
withContractAs' :: AppConfig -> Int -> TFContract UnbalancedTx -> TestRunner String TFError Slot
withContractAs' cfg i c = PlutipContract.withContractAs i . const $ runTFContract cfg (c >>= submitAndWait)

-- | Useful for debugging
_printValue :: forall a. ExecutionResult String TFError (a, NonEmpty Value) -> TestRunnerM ()
_printValue result =
  either
    (const $ liftIO $ putStrLn "(printValue) Left")
    (\(_, values) -> liftIO . putStrLn $ "\nValue: " <> show values <> "\n\n")
    $ outcome result
