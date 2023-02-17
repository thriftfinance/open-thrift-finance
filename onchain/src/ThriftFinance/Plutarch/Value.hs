{-# OPTIONS_GHC -Wno-orphans #-}

-- This module should be made redundant by plutarch-extra utilities eventually.
module ThriftFinance.Plutarch.Value (
  PAssetClass,
  ponlyWithAda,
  pisSingletonTokenMap,
  passertSingleAssetFor,
  pwithoutCurrency,
  phasAnyOfCurrency,
) where

import Plutarch.Api.V1 (PCurrencySymbol, PTokenName, PTuple, PValue (PValue))
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Unsafe qualified as PUNSAFE

import ThriftFinance.Plutarch.AssocMap (phasAnyOf, pwithoutKey)

import PlutusLedgerApi.V1.Value (adaSymbol, adaToken)

type PAssetClass = PTuple PCurrencySymbol PTokenName

{- | Query whether any of the given curr syms exist in given 'PValue'.
 FIXME: This currently assumes no zero valued assets exist in the given 'PValue'.
 This is supposed to be fixed by using upstream value API which is being finalized.
-}
phasAnyOfCurrency ::
  PIsListLike list (PAsData PCurrencySymbol) =>
  Term s (list (PAsData PCurrencySymbol) :--> PValue keys amounts :--> PBool)
phasAnyOfCurrency = phoistAcyclic $
  plam $ \css v -> phasAnyOf # css #$ pto v

-- | Remove assets with given currency symbol from given 'PValue'.
pwithoutCurrency :: Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PValue keys amounts)
pwithoutCurrency = phoistAcyclic $
  plam $ \cs v -> pcon $ PValue $ pwithoutKey # cs # pto v

-- | Ensure there is only one asset (one token name, positive amount) for the given currency symbol within given 'PValue'.
passertSingleAssetFor :: ClosedTerm PString -> Term s (PCurrencySymbol :--> PValue keys amounts :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
passertSingleAssetFor s = phoistAcyclic $
  plam $ \cs val ->
    let tokMap = pmatch (PMap.plookup # cs # pto val) $ \case
          PJust x -> x
          PNothing -> PUNSAFE.punsafeDowncast $ pconstant []
     in pmatch (pto tokMap) $ \case
          PNil -> ptraceError s
          PCons x xs ->
            pif
              (pnull # xs #&& 1 #<= pfromData (psndBuiltin # x))
              x
              $ ptraceError s

-- | Ensure given 'PValue' only contains some amount of ada and the given amount of the given asset.
ponlyWithAda :: Term s (PAsData PCurrencySymbol :--> PAsData PTokenName :--> PAsData PInteger :--> PValue keys amounts :--> PBool)
ponlyWithAda = phoistAcyclic $
  plam $ \cs tk i v ->
    let csTkAscList = pto $ pto v
     in pelimList
          ( \x xs ->
              -- Ensure the first asset is some ada (don't care how much).
              pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon PNothing # x
                -- Ensure the second asset is the given cs/tk, with given amount.
                #&& pelimList
                  -- Also ensure these are the only 2 assets.
                  (\y ys -> pnull # ys #&& pisSingletonTokenMap # cs # tk # pcon (PJust i) # y)
                  (pconstant False)
                  xs
          )
          (pconstant False)
          csTkAscList

{- | Given a pair of curr sym and its token map from a 'PValue', as well as expected curr sym, token name,
 and optionally the expected amount, determine whether the provided pair is indeed a singular asset with
 matching amount (if given).
 ==== Example ====
 @
 let v = pconstant $ Value.fromList [(adaSymbol, [(adaToken, 42)])]
 in pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon PNothing # v
 -- _^ yields True
 let v = pconstant $ Value.fromList [(adaSymbol, [(adaToken, 42)])]
 in pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon (PJust 42) # v
 -- _^ yields True
 let v = pconstant $ Value.fromList [(adaSymbol, [(adaToken, 2)])]
 in pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon (PJust 42) # v
 -- _^ yields False (incorrect amount)
 let v = pconstant $ Value.fromList [(adaSymbol, [(someOtherToken, 2)])]
 in pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon (PJust 42) # v
 -- _^ yields False (incorrect token)
 let v = pconstant $ Value.fromList [(adaSymbol, [(adaToken, 42), (someOtherToken, 2)])]
 in pisSingletonTokenMap # pconstantData adaSymbol # pconstantData adaToken # pcon (PJust 42) # v
 -- _^ yields False (multiple tokens)
 @
-}
pisSingletonTokenMap ::
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PMaybe (PAsData PInteger)
        :--> PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap keys PTokenName PInteger))
        :--> PBool
    )
pisSingletonTokenMap = phoistAcyclic $
  plam $ \cs tk maybeD ctm ->
    let actualCs = pfstBuiltin # ctm
        actualTkMap = pfromData $ psndBuiltin # ctm
     in actualCs #== cs
          #&& pelimList
            ( \t ts ->
                pnull # ts #&& pfstBuiltin # t #== tk
                  #&& pmatch
                    maybeD
                    ( \case
                        PNothing -> pconstant True
                        PJust d -> d #== psndBuiltin # t
                    )
            )
            (pconstant False)
            (pto actualTkMap)
