module Spec.Plutip.Common (
  TestRunnerM,
  TestRunner,
  dummyResult,
  validatorAddress,
  scriptCurrencySymbol,
  initConfig,
  runTFContract,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Row (Empty)
import Ledger (Address (Address, addressCredential, addressStakingCredential))
import Ledger.Ada qualified as Ada
import Ledger.Address (scriptHashAddress)
import Plutus.Contract (Contract)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol, validatorHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (Value)
import Scripts.V1.Deserialize (readConstTrueValidator, readThriftTkMp, readThriftValidator, readTrivialRewardTkF, readTrivialTkF)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (BpiWallet))
import Test.Plutip.Internal.Types (ClusterEnv, ExecutionResult)
import ThriftFinance.Common.Types (AppConfig (AppConfig), TargetProtocolInfo (TargetProtocolInfo, tpi'PassiveDepositCurrency, tpi'PassiveDepositName), tf'TokenMp, tf'Validator, tf'offchainChecksEnabled, tf'targetProtocolInfo, tf'targetProtocolMintingPolicies, tf'targetProtocolValidators, tpi'ActiveDepositCurrency, tpi'ClaimReturnAfter, tpi'DepositTarget, tpi'Fee, tpi'FeeRecipient, tpi'LockingScript, tpi'NumberOfContributions)
import ThriftFinance.Offchain.Error (TFError)
import ThriftFinance.Offchain.TFContract (TFContract)

type TestRunnerM = ReaderT (ClusterEnv, NonEmpty BpiWallet) IO
type TestRunner w e a = TestRunnerM (ExecutionResult w e (a, NonEmpty Value))

-- | A dummy test result, useful while writing tests
dummyResult :: forall w e. ExecutionResult w e ((), NonEmpty Value)
dummyResult = error "dummy result"

-- | Gets the address of a validator
validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash

{- | This code sets up the configuration of the pool using a random
 BPI wallet as the fee recipient
-}
initConfig :: TestRunnerM AppConfig
initConfig = do
  -- Get recipient wallet (use the first wallet)
  (BpiWallet pkh _ _) :| _ <- asks snd
  let recipient :: Address
      recipient =
        Address
          { addressCredential = PubKeyCredential pkh
          , addressStakingCredential = Nothing
          }
  -- Load scripts
  thriftTkMp <- liftIO readThriftTkMp
  thriftValidator <- liftIO readThriftValidator <*> pure (scriptCurrencySymbol thriftTkMp)
  targetValidator <- liftIO readConstTrueValidator
  targetTkMp <- liftIO readTrivialTkF <*> pure targetValidator
  targetRewardTkMp <- liftIO readTrivialRewardTkF <*> pure (scriptCurrencySymbol targetTkMp)
  -- Initialise target protocol info
  let tpi =
        TargetProtocolInfo
          { tpi'LockingScript = validatorAddress targetValidator
          , tpi'DepositTarget = 1_000 * 1_000_000
          , tpi'NumberOfContributions = 4
          , tpi'PassiveDepositCurrency = Ada.adaSymbol
          , tpi'PassiveDepositName = Ada.adaToken
          , tpi'ActiveDepositCurrency = scriptCurrencySymbol targetTkMp
          , tpi'ClaimReturnAfter = 1 -- dummy value
          , tpi'Fee = Ada.adaValueOf 5
          , tpi'FeeRecipient = recipient
          }
  -- Return config
  pure
    AppConfig
      { tf'TokenMp = thriftTkMp
      , tf'Validator = thriftValidator
      , tf'targetProtocolInfo = tpi
      , tf'targetProtocolValidators = targetValidator
      , tf'targetProtocolMintingPolicies = [targetTkMp, targetRewardTkMp]
      , tf'offchainChecksEnabled = True
      }

-- | A convenient  function for executing `TFContract`s in the `TestRunner` monad
runTFContract :: forall a. AppConfig -> TFContract a -> Contract String Empty TFError a
runTFContract = flip runReaderT
