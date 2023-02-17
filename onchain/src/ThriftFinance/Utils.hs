module ThriftFinance.Utils (
  inputsAmountOf,
  outputsAmountOf,
  TxInfoFields,
  TxInfoF,
  TPInfoFields,
  TPInfoF,
  PTFDatumFields,
) where

import Plutarch.Api.V1 (
  AmountGuarantees (NoGuarantees),
  KeyGuarantees (Unsorted),
  PAddress,
  PCurrencySymbol,
  PTokenName,
  PTxInInfo,
  PTxOut,
  PValue,
 )

import Plutarch.Extra.TermCont (
  pletFieldsC,
 )

import Plutarch.Api.V1.Contexts (PTxInfo)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.DataRepr (HRec, PDataFields (PFields))
import Plutarch.DataRepr.Internal.Field (Bindings, BoundTerms)
import Plutarch.Prelude ()

{- | Folds over the outputs of a transaction and returns the quantity of a
     given asset
-}
outputsAmountOf :: forall (s :: S). Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxOut :--> PInteger)
outputsAmountOf = phoistAcyclic $
  plam $ \cs tn outputs -> unTermCont $ do
    pure $
      pfoldl
        # plam
          ( \acc txOut -> unTermCont $ do
              value <- getField @"value" <$> pletFieldsC @'["value"] txOut
              let lovelace = pvalueOf # value # cs # tn
              pure $ acc + lovelace
          )
        # 0
        # outputs

{- | Folds over the inputs of a transaction and returns the quantity of a
 | given asset
-}
inputsAmountOf :: forall (s :: S). Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxInInfo :--> PInteger)
inputsAmountOf = phoistAcyclic $
  plam $ \cs tn inputs ->
    pfoldl
      # plam
        ( \acc input ->
            let value = pfield @"value" #$ pfield @"resolved" # input
                lovelace = pvalueOf # value # cs # tn
             in acc + lovelace
        )
      # 0
      # inputs

-- TYPE SYNONYMS --
type TxInfoFields = '["inputs", "outputs", "mint", "datums", "signatories"]

type TxInfoF (s :: S) =
  HRec
    ( BoundTerms
        (PFields (PAsData PTxInfo))
        (Bindings (PFields (PAsData PTxInfo)) TxInfoFields)
        s
    )

type TPInfoFields =
  [ "lockingScript"
  , "depositTarget"
  , "numberOfContributions"
  , "passiveDepositCurrency"
  , "passiveDepositName"
  , "activeDepositCurrency"
  , "claimReturnAfter"
  , "fee"
  , "feeRecipient"
  ]

type TPInfoF (s :: S) =
  HRec
    ( BoundTerms
        ( PFields
            ( PDataSum
                '[ '[ "lockingScript" ':= PAddress
                    , "depositTarget" ':= PInteger
                    , "numberOfContributions" ':= PInteger
                    , "passiveDepositCurrency" ':= PCurrencySymbol
                    , "passiveDepositName" ':= PTokenName
                    , "activeDepositCurrency" ':= PCurrencySymbol
                    , "claimReturnAfter" ':= PInteger
                    , "fee" ':= PValue 'Unsorted 'NoGuarantees
                    , "feeRecipient" ':= PAddress
                    ]
                 ]
            )
        )
        ( Bindings
            ( PFields
                ( PDataSum
                    '[ '[ "lockingScript" ':= PAddress
                        , "depositTarget" ':= PInteger
                        , "numberOfContributions" ':= PInteger
                        , "passiveDepositCurrency" ':= PCurrencySymbol
                        , "passiveDepositName" ':= PTokenName
                        , "activeDepositCurrency" ':= PCurrencySymbol
                        , "claimReturnAfter" ':= PInteger
                        , "fee" ':= PValue 'Unsorted 'NoGuarantees
                        , "feeRecipient" ':= PAddress
                        ]
                     ]
                )
            )
            TPInfoFields
        )
        s
    )

type PTFDatumFields = ["targetProtocol", "depositState", "deposits"]
