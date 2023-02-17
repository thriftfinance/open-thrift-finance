module Contract.Utils (guardMaybe, guardCond, datumFromHash) where

import Control.Monad.Except (throwError)

import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Scripts (Datum (getDatum), DatumHash)
import PlutusTx (FromData, fromBuiltinData)

-- | Specialized 'fromMaybe' under the contract monad; throws error in case of 'Nothing'.
guardMaybe :: e -> Contract w s e (Maybe a) -> Contract w s e a
guardMaybe err c = do
  m <- c
  maybe (throwError err) pure m

-- | Conditional failure under the contract monad; throws error if condition is false.
guardCond :: e -> Bool -> Contract w s e ()
guardCond err c = if c then pure () else Contract.throwError err

-- | Retrieves a datum from the chain index based on its hash.
datumFromHash :: (FromData a, AsContractError e) => DatumHash -> Contract w s e (Maybe a)
datumFromHash h = do
  md <- Contract.datumFromHash h
  pure $ md >>= PlutusTx.fromBuiltinData . getDatum
