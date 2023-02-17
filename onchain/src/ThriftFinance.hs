module ThriftFinance (
  constTrueValidator,
  constTrueScript,
  otherConstTrueValidator,
  ptrivialTkf,
  trivialTkM,
  ptrivialRewardTkf,
  trivialRewardTkM,
  otherConstTrueScript,
  pconstTrueValidator,
  pthriftFinanceToken,
  pthriftFinanceTokenMp,
  thriftFinanceTokenMP,
  pthriftFinanceValidator,
) where

import ThriftFinance.TrivialProtocol.Token (
  ptrivialTkf,
  trivialTkM,
 )

import ThriftFinance.TrivialProtocol.RewardToken (
  ptrivialRewardTkf,
  trivialRewardTkM,
 )

import ThriftFinance.Token.MintingPolicy (
  pthriftFinanceToken,
  pthriftFinanceTokenMp,
  thriftFinanceTokenMP,
 )

import Plutarch.Api.V1 (
  PScriptContext,
  PValidator,
  mkValidator,
 )

import Plutarch.Extra.TermCont (
  pletC,
 )

import Plutarch (Config (Config), TracingMode (NoTracing))
import Plutarch.Prelude ()

import PlutusLedgerApi.V1.Scripts (Validator)

import ThriftFinance.Plutarch.Scripts.V1 (wrapValidator)
import ThriftFinance.Script.Validator (pthriftFinanceValidator)

-- | Always passes.
constTrueValidator :: Validator
constTrueValidator = mkValidator (Config NoTracing) $ wrapValidator constTrueScript

-- | Always passes.
constTrueScript :: forall (s :: S). Term s (PData :--> PData :--> PScriptContext :--> PUnit)
constTrueScript = plam $ \_ _ _ -> pconstant ()

{- | A different validator (with a distinct address) that always passes. Used
for testing
-}
otherConstTrueValidator :: forall (s :: S). Term s (PData :--> PData :--> PScriptContext :--> PUnit)
otherConstTrueValidator = plam $ \_ _ _ -> unTermCont $ do
  x <- pletC (1948 :: Term s PInteger)
  pure $ plam (const (pconstant ())) # x

-- | Always passes.
pconstTrueValidator :: ClosedTerm PValidator
pconstTrueValidator = plam $ \_ _ _ -> popaque $ pconstant ()

otherConstTrueScript :: ClosedTerm PValidator
otherConstTrueScript = plam $ \dat red ctx ->
  popaque $ otherConstTrueValidator # dat # red # ctx
