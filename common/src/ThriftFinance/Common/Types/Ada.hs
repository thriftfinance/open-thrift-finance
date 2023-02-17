{-# LANGUAGE CPP #-}

{- This module exists because 'Plutus.V1.Ledger.Ada' has been removed from
'plutus' in newer versions.
-}
module ThriftFinance.Common.Types.Ada (Ada (Ada), toLovelace, toValue, fromValue) where

import Data.Fixed (Fixed (MkFixed), Micro)

#ifdef NEW_LEDGER_NAMESPACE

import PlutusLedgerApi.V1.Value (Value, adaSymbol, adaToken)
import qualified PlutusLedgerApi.V1.Value as Value

#else

import Plutus.V1.Ledger.Api (Value, adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Value as Value

#endif


-- | Ada represented with a 'Micro' value.
newtype Ada = Ada Micro
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

-- | Convert Ada amount to its corresponding Lovelace unit.
toLovelace :: Ada -> Integer
toLovelace (Ada (MkFixed i)) = i

-- | Create a 'Value' containing given amount of Ada.
toValue :: Ada -> Value
toValue = Value.singleton adaSymbol adaToken . toLovelace

-- | Obtain the 'Ada' amount contained in a 'Value'.
fromValue :: Value -> Ada
fromValue v = Ada . MkFixed $ Value.valueOf v adaSymbol adaToken
