module ThriftFinance.Config (
  constTrueValidatorAddress,
  otherConstTrueValidatorAddress,
) where

import PlutusLedgerApi.V1.Address (
  Address,
  scriptHashAddress,
 )

import Plutarch (Config (Config, tracingMode), TracingMode (NoTracing))

import Plutarch.Api.V1 (mkValidator, validatorHash)

import ThriftFinance (constTrueValidator, otherConstTrueScript)

cfg :: Config
cfg = Config {tracingMode = NoTracing}

constTrueValidatorAddress :: Address
constTrueValidatorAddress = scriptHashAddress $ validatorHash constTrueValidator

otherConstTrueValidatorAddress :: Address
otherConstTrueValidatorAddress = scriptHashAddress $ validatorHash $ mkValidator cfg otherConstTrueScript
