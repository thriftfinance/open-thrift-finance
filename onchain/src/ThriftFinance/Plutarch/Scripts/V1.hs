module ThriftFinance.Plutarch.Scripts.V1 (wrapMintingPolicy, wrapValidator, wrapValidatorNoRedeemer) where

import Plutarch.Api.V1 (PMintingPolicy, PScriptContext, PValidator)

import ThriftFinance.Plutarch.TermCont (pverifyDataC)

-- | Wrap a typed minting policy term with data verification.
wrapMintingPolicy ::
  forall redm any s.
  (PTryFrom PData redm, PIsData redm) =>
  Term s (redm :--> PScriptContext :--> any) ->
  Term s PMintingPolicy
wrapMintingPolicy pf =
  plam $ \redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @redm redm'
    pure . popaque $ pf # redm # ctx

-- | Wrap a typed validator term with data verification.
wrapValidator ::
  forall datm redm s.
  ( PTryFrom PData datm
  , PTryFrom PData redm
  , PIsData datm
  , PIsData redm
  ) =>
  Term s (datm :--> redm :--> PScriptContext :--> PUnit) ->
  Term s PValidator
wrapValidator pf =
  plam $ \datm' redm' ctx -> unTermCont $ do
    datm <- pverifyDataC @datm datm'
    redm <- pverifyDataC @redm redm'
    pure . popaque $ pf # datm # redm # ctx

{- | Wrap a typed validator term with data verification. Redeemer is not
 validated
-}
wrapValidatorNoRedeemer ::
  forall datm s.
  ( PTryFrom PData datm
  , PIsData datm
  ) =>
  Term s (datm :--> PData :--> PScriptContext :--> PUnit) ->
  Term s PValidator
wrapValidatorNoRedeemer pf =
  plam $ \datm' redm ctx -> unTermCont $ do
    datm <- pverifyDataC @datm datm'
    pure . popaque $ pf # datm # redm # ctx
