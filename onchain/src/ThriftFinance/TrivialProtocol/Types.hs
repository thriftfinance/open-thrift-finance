{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ThriftFinance.TrivialProtocol.Types (
  PTrivialTkMpRedeemer,
  PTrivialRewardTkMpRedeemer,
) where

import ThriftFinance.Common.Types (TrivialRewardTkMpRedeemer, TrivialTkMpRedeemer)

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))

-- | Trivial Token Minting Policy Redeemer.
data PTrivialTkMpRedeemer s
  = PMintTk (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PTrivialTkMpRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PTrivialTkMpRedeemer)
instance PUnsafeLiftDecl PTrivialTkMpRedeemer where
  type PLifted PTrivialTkMpRedeemer = TrivialTkMpRedeemer

deriving via
  DerivePConstantViaData TrivialTkMpRedeemer PTrivialTkMpRedeemer
  instance
    PConstantDecl TrivialTkMpRedeemer

-- | Trivial Reward Token Minting Policy Redeemer.
data PTrivialRewardTkMpRedeemer s
  = PMintRewardTk (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData, PDataFields)

instance DerivePlutusType PTrivialRewardTkMpRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PTrivialRewardTkMpRedeemer)
instance PUnsafeLiftDecl PTrivialRewardTkMpRedeemer where
  type PLifted PTrivialRewardTkMpRedeemer = TrivialRewardTkMpRedeemer

deriving via
  DerivePConstantViaData TrivialRewardTkMpRedeemer PTrivialRewardTkMpRedeemer
  instance
    PConstantDecl TrivialRewardTkMpRedeemer
