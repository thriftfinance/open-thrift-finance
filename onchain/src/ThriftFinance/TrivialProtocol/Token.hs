module ThriftFinance.TrivialProtocol.Token (
  trivialTkM,
  ptrivialTkf,
) where

import ThriftFinance.Utils (
  inputsAmountOf,
  outputsAmountOf,
 )

import Plutarch.Api.V1 (
  PAddress,
  PCredential (PPubKeyCredential),
  PCurrencySymbol,
  PMintingPolicy,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxInfo (PTxInfo),
  PTxOut,
 )

import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
 )

import Plutarch.Api.V1.Address (PCredential (PScriptCredential))
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Prelude ()

import ThriftFinance.Common.Constants (trivialTokenName)
import ThriftFinance.Plutarch.Patterns (pfield0)
import ThriftFinance.Plutarch.TermCont (pconstantC, pifC)
import ThriftFinance.Types (
  PTrivialTkMpRedeemer,
 )

{- | Trivial minting policy that allows the user to mint 1:1 tokens with the
 amount of lovelace deposited at a specific address. It also allows a user
 to burn the tokens and retrieve the lovelace deposited.
-}
ptrivialTkf :: forall (s :: S). Term s (PByteString :--> PTrivialTkMpRedeemer :--> PScriptContext :--> PUnit)
ptrivialTkf = plam $ \scriptHash _ ctx' -> unTermCont $ do
  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'

  PTxInfo info <- pmatchC $ getField @"txInfo" ctx
  txInfo <- pletFieldsC @'["mint", "outputs", "inputs"] info

  PMinting mintFields <- pmatchC $ getField @"purpose" ctx
  let ownSymbol :: Term s PCurrencySymbol
      ownSymbol = pfield @"_0" # mintFields

  trivialTkAmount <-
    pletC $
      pvalueOf
        # txInfo.mint
        # ownSymbol
        # pconstant trivialTokenName

  let outputs :: Term s (PBuiltinList PTxOut)
      outputs = txInfo.outputs
      inputs :: Term s (PBuiltinList PTxInInfo)
      inputs = getField @"inputs" txInfo

  pifC
    (0 #< trivialTkAmount)
    (checkMinting scriptHash trivialTkAmount outputs)
    (checkBurning scriptHash trivialTkAmount inputs)
  where
    checkMinting :: Term s PByteString -> Term s PInteger -> Term s (PBuiltinList PTxOut) -> Term s PUnit
    checkMinting scriptHash mintAmount outputs = unTermCont $ do
      addrOutputs <- pletC $
        unTermCont $ do
          pure $
            pfilter
              # plam
                ( \txOut ->
                    let addr = pfield @"address" # txOut
                     in equalScriptAddress # scriptHash # addr
                )
              # outputs
      -- get total lovelace paid
      totalLovelace <- pletC $ outputsAmountOf # padaSymbol # padaToken # addrOutputs

      pguardC "The amount of minted tokens does not correspond with deposited lovelace" $
        mintAmount #== totalLovelace
      pconstantC ()
    checkBurning :: Term s PByteString -> Term s PInteger -> Term s (PBuiltinList PTxInInfo) -> Term s PUnit
    checkBurning scriptHash burntAmount outputs = unTermCont $ do
      -- filter only the inputs from our given address
      let addrInputs =
            pfilter
              # plam
                ( \info ->
                    let payingFrom = pfield @"address" #$ pfield @"resolved" # info
                     in equalScriptAddress # scriptHash # payingFrom
                )
              # outputs
      -- get total lovelace withdrawn
      let totalLovelace = inputsAmountOf # padaSymbol # padaToken # addrInputs

      pguardC "The amount of burnt tokens does not correspond with withdrawn lovelace" $
        burntAmount #== -totalLovelace
      pconstantC ()

    -- Returns true if the address is a script address and it has the same hash
    equalScriptAddress :: Term s (PByteString :--> PAddress :--> PBool)
    equalScriptAddress = phoistAcyclic $
      plam $ \scriptHash addr ->
        let cred = pfield @"credential" # addr
         in pmatch cred $ \case
              PPubKeyCredential _ -> pconstant False
              PScriptCredential (pfromData . pto . pfield0 -> hash) -> hash #== scriptHash

trivialTkM :: ClosedTerm (PByteString :--> PMintingPolicy)
trivialTkM = plam $ \scriptHash red' ctx -> unTermCont $ do
  (red, _) <- ptryFromC red'
  pure $ popaque $ ptrivialTkf # scriptHash # red # ctx
