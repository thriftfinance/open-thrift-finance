{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ThriftFinance.Token.Types (
  PTFRedeemer (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import ThriftFinance.Common.Types (TFRedeemer)

data PTFRedeemer (s :: S)
  = PInitialise (Term s (PDataRecord '[]))
  | PDeposit (Term s (PDataRecord '[]))
  | PActivate (Term s (PDataRecord '[]))
  | PReturn (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData)

instance DerivePlutusType PTFRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTFRedeemer where
  type PLifted PTFRedeemer = TFRedeemer

deriving via
  DerivePConstantViaData TFRedeemer PTFRedeemer
  instance
    PConstantDecl TFRedeemer
