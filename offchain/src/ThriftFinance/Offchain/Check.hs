module ThriftFinance.Offchain.Check (
  Check,
  runCheck,
  mkCheck,
) where

import Control.Monad (when)
import ThriftFinance.Offchain.TFContract (TFContract, checksEnabled)

{- | Type of offchain validation checks. These can be disabled via the
 | `tf'offchainChecksEnabled` flag in `AppConfig`
-}
newtype Check = Check {_runCheck :: TFContract ()}

-- | Execute some validation if checks are enabled
runCheck :: Check -> TFContract ()
runCheck check = do
  enabled <- checksEnabled
  when enabled $ _runCheck check

-- | Smart constructor for offchain checks
mkCheck :: TFContract () -> Check
mkCheck = Check
