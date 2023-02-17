module ThriftFinance.Offchain.Contracts.Cancel (
  cancelContribution,
) where

import Ledger.Constraints (UnbalancedTx)
import ThriftFinance.Offchain.TFContract (TFContract)

-- | FIXME: Cancel a contribution to a Thrift Finance peer lending pool
cancelContribution :: TFContract UnbalancedTx
cancelContribution = undefined
