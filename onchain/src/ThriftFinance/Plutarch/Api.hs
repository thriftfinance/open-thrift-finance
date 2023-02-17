module ThriftFinance.Plutarch.Api (
  psingleContinuingOutput,
  psingleContinuingOutput',
  padaSym,
  padaTk,
  padaSymData,
  padaTkData,
) where

import Plutarch.Api.V1 (PAddress, PCurrencySymbol, PTokenName, PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Extra.Api (pfindOwnInput)
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken)

import ThriftFinance.Plutarch.List (psingleMatch)

-- | Ensure there is only one single continuing output and obtain it.
psingleContinuingOutput ::
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PTxOutRef
        :--> PBuiltinList (PAsData PTxOut)
        :--> PMaybe (PAsData PTxOut)
    )
psingleContinuingOutput = phoistAcyclic $
  plam $ \inputs outRef ->
    pmatch (pfindOwnInput # (pmap # (plam $ \x -> pfromData x) # inputs) # outRef) $ \case
      PJust tx -> psingleContinuingOutput' # tx
      PNothing ->
        ptraceError "can't get any continuing outputs"

-- | Like 'psingleContinuingOutput', but takes in the own input instead of computing it.
psingleContinuingOutput' :: Term s (PTxInInfo :--> PBuiltinList (PAsData PTxOut) :--> PMaybe (PAsData PTxOut))
psingleContinuingOutput' = phoistAcyclic $
  plam $ \ownInp outputs ->
    let resolved = pfield @"resolved" # ownInp
        outAddr = pfield @"address" # resolved
     in psingleMatch # (matches # outAddr) # outputs
  where
    matches :: Term s (PAddress :--> PAsData PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

-- | 'adaSym' as Plutarch constant
padaSym :: ClosedTerm PCurrencySymbol
padaSym = pconstant adaSymbol

-- | 'adaToken' as Plutarch constant
padaTk :: ClosedTerm PTokenName
padaTk = pconstant adaToken

-- | 'adaSym' as Plutarch data encoded constant.
padaSymData :: ClosedTerm (PAsData PCurrencySymbol)
padaSymData = pconstantData adaSymbol

-- | 'adaToken' as Plutarch data encoded constant.
padaTkData :: ClosedTerm (PAsData PTokenName)
padaTkData = pconstantData adaToken
