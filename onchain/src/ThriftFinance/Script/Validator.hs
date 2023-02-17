module ThriftFinance.Script.Validator (pthriftFinanceValidatorF, pthriftFinanceValidator) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PScriptContext,
  PValidator,
 )

import Plutarch.Prelude ()

import Plutarch.Extra.TermCont (pguardC, pletFieldsC)
import ThriftFinance.Plutarch.Scripts.V1 (wrapValidatorNoRedeemer)
import ThriftFinance.Plutarch.TermCont (pconstantC)
import ThriftFinance.Plutarch.Value (phasAnyOfCurrency)
import ThriftFinance.Script.Types (PTFDatum)

{- | Thrift Finance Validator logic. The validator defers all checks to the
 minting policy by only validating that a TF token is minted/burnt
-}
pthriftFinanceValidatorF :: forall (s :: S). Term s (PCurrencySymbol :--> PTFDatum :--> PData :--> PScriptContext :--> PUnit)
pthriftFinanceValidatorF = plam $ \tfCs _dat _ ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo"] ctx
  txInfoF <- pletFieldsC @'["mint"] ctxF.txInfo
  pguardC "(pthriftFinanceValidatorF) no TFToken minted/burnt" $
    phasAnyOfCurrency @PList # (pcons # pdata tfCs # pnil) # txInfoF.mint
  pconstantC ()

pthriftFinanceValidator ::
  forall (s :: S). Term s (PCurrencySymbol :--> PValidator)
pthriftFinanceValidator = plam $ \cs -> wrapValidatorNoRedeemer $ pthriftFinanceValidatorF # cs
