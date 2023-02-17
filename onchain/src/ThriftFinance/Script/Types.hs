{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ThriftFinance.Script.Types (
  PTFDatum (..),
  PTargetProtocolInfo (..),
  PDepositState (..),
  PDeposits (..),
) where

import Data.Bifunctor (bimap)
import Data.Traversable (for)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.Api.V1 (AmountGuarantees (NoGuarantees), KeyGuarantees (Sorted, Unsorted), PAddress, PCurrencySymbol, PMap, PMaybeData, PPubKeyHash, PStakingCredential, PTokenName, PValue)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr), PUnsafeLiftDecl (PLifted))
import PlutusTx (Data, fromData, toData)
import ThriftFinance.Common.Types (
  DepositState,
  OrdMap,
  TFDatum,
  TargetProtocolInfo,
  fromList,
  toList,
 )

data PTFDatum (s :: S)
  = PTFDatum
      ( Term
          s
          ( PDataRecord
              '[ "targetProtocol" ':= PTargetProtocolInfo
               , "depositState" ':= PDepositState
               , "deposits" ':= PDeposits
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData)

data PTargetProtocolInfo (s :: S)
  = PTargetProtocolInfo
      ( Term
          s
          ( PDataRecord
              '[ "lockingScript" ':= PAddress
               , "depositTarget" ':= PInteger
               , "numberOfContributions" ':= PInteger
               , "passiveDepositCurrency" ':= PCurrencySymbol
               , "passiveDepositName" ':= PTokenName
               , "activeDepositCurrency" ':= PCurrencySymbol
               , "claimReturnAfter" ':= PInteger
               , "fee" ':= PValue 'Unsorted 'NoGuarantees
               , "feeRecipient" ':= PAddress
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData)

---- TESTING GROUND ----
-- tpInfo = TargetProtocolInfo
-- { tpi'LockingScript = "aeaeaeaeaeaea"
-- , tpi'DepositTarget = 1_000 * 1_000_000
-- , tpi'NumberOfContributions = 4
-- , tpi'PassiveDepositCurrency = Value.adaSymbol
-- , tpi'PassiveDepositName= Value.adaToken
-- , tpi'ActiveDepositCurrency = "aeaeaeaeaeaea"
-- , tpi'ClaimReturnAfter = 1 -- dummy value
-- , tpi'Fee = Value.singleton Value.adaSymbol Value.adaToken 5_000_000
-- , tpi'FeeRecipient = "aeaeaeaaeaea"
-- }
--
-- ptpInfo = pconstant tpInfo

-- deps :: Deposits
-- deps = Deposits $ fromList
--   [(PubKeyHash "aeaeaeaeae",
--      (Just $ StakingHash $ PubKeyCredential $ PubKeyHash "aeaeaeaeae",
--      55))]
--
-- deps' = Deposits $ fromList []
--
-- pdeps :: forall (s :: S). Term s PDeposits
-- pdeps = pcon $ PDeposits $ pempty

-- Idempotence
-- x = pfromData $ ptryFrom @(PAsData PDeposits) (pforgetData $ pdata pdeps) fst
--
-- y = pfromData $ ptryFrom @(PAsData PDeposits) (pconstant $ toData deps') fst

------------------------

data PDepositState (s :: S)
  = PPendingDepositState (Term s (PDataRecord '[]))
  | PActiveDepositState (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq, PTryFrom PData)

newtype PDeposits (s :: S)
  = PDeposits
      ( Term
          s
          ( PMap
              'Sorted
              PPubKeyHash
              ( PBuiltinPair
                  (PAsData (PMaybeData PStakingCredential))
                  (PAsData PInteger)
              )
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PlutusType, PIsData, PEq)
deriving anyclass instance PTryFrom PData (PAsData PDeposits)

instance DerivePlutusType PTFDatum where type DPTStrat _ = PlutusTypeData

instance DerivePlutusType PTargetProtocolInfo where type DPTStrat _ = PlutusTypeData

instance DerivePlutusType PDepositState where type DPTStrat _ = PlutusTypeData

instance DerivePlutusType PDeposits where type DPTStrat _ = PlutusTypeNewtype

instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap 'Sorted k v)
  where
  type PLifted (PMap 'Sorted k v) = OrdMap (PLifted k) (PLifted v)

instance PUnsafeLiftDecl PTFDatum where
  type PLifted PTFDatum = TFDatum

instance PUnsafeLiftDecl PTargetProtocolInfo where
  type PLifted PTargetProtocolInfo = TargetProtocolInfo

instance PUnsafeLiftDecl PDepositState where
  type PLifted PDepositState = DepositState

instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (OrdMap k v)
  where
  type PConstantRepr (OrdMap k v) = [(Data, Data)]
  type PConstanted (OrdMap k v) = PMap 'Sorted (PConstanted k) (PConstanted v)
  pconstantToRepr m = bimap toData toData <$> toList m
  pconstantFromRepr m = fmap fromList $
    for m $ \(x, y) -> do
      x' <- fromData x
      y' <- fromData y
      Just (x', y')

deriving via
  DerivePConstantViaData TFDatum PTFDatum
  instance
    PConstantDecl TFDatum

deriving via
  DerivePConstantViaData TargetProtocolInfo PTargetProtocolInfo
  instance
    PConstantDecl TargetProtocolInfo

deriving via
  DerivePConstantViaData DepositState PDepositState
  instance
    PConstantDecl DepositState
