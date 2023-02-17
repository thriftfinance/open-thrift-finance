module ThriftFinance.TrivialProtocol.RewardToken (
  trivialRewardTkM,
  ptrivialRewardTkf,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PMintingPolicy,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
 )

import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
 )

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Prelude ()

import ThriftFinance.Common.Constants (trivialRewardTokenName, trivialTokenName)
import ThriftFinance.Plutarch.Patterns (pfield0)
import ThriftFinance.Plutarch.TermCont (pconstantC)
import ThriftFinance.TrivialProtocol.Types (PTrivialRewardTkMpRedeemer)

{- | Minting policy for the trivial reward token. A reward token may only be minted
 for every 5 trivial tokens *burnt*
-}
ptrivialRewardTkf :: forall (s :: S). Term s (PCurrencySymbol :--> PTrivialRewardTkMpRedeemer :--> PScriptContext :--> PUnit)
ptrivialRewardTkf = plam $ \trivialTkCs _ ctx' -> unTermCont $ do
  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
  txInfo <- pletFieldsC @'["mint", "inputs", "outputs"] ctx.txInfo
  -- We get the amount being minted
  PMinting (pfield0 -> ownSymbol) <- pmatchC ctx.purpose
  rewardsAmount <- pletC $ pvalueOf # txInfo.mint # ownSymbol # pconstant trivialRewardTokenName
  -- Make sure that the token is being minted and not burnt
  pguardC "ptrivialRewardTkf: reward tokens may only be minted, not burnt" $
    0 #< rewardsAmount
  -- Validate the amount of reward tokens minted against the amount of trivial
  -- tokens burnt
  pguardC "ptrivialRewardMintHandler: burnt tokens condition not satisfied" $
    pany
      # containsTrivialToken trivialTkCs rewardsAmount
      # txInfo.inputs
  ThriftFinance.Plutarch.TermCont.pconstantC ()
  where
    containsTrivialToken :: Term s PCurrencySymbol -> Term s PInteger -> Term s (PTxInInfo :--> PBool)
    containsTrivialToken trivialTkCs rewardsAmount = plam $ \txInInfo ->
      let inputValue = pfield @"value" #$ pfield @"resolved" # txInInfo
          burntAmount = pvalueOf # inputValue # trivialTkCs # pconstant trivialTokenName
       in burntAmount #== rewardsAmount * 5

trivialRewardTkM :: ClosedTerm (PCurrencySymbol :--> PMintingPolicy)
trivialRewardTkM = plam $ \trivialTkCs red' ctx -> unTermCont $ do
  (red, _) <- ptryFromC red'
  pure $ popaque $ ptrivialRewardTkf # trivialTkCs # red # ctx
