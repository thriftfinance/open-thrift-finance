module ThriftFinance.Token.MintingPolicy (
  pthriftFinanceToken,
  pthriftFinanceTokenMp,
  thriftFinanceTokenMP,
) where

import Plutarch.Api.V1 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress,
  PCredential (PPubKeyCredential),
  PDatum (PDatum),
  PDatumHash,
  PMaybeData (PDJust),
  PMintingPolicy,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting),
  PStakingCredential,
  PTokenName,
  PTuple,
  PTxInInfo,
  PTxOut,
  PValue,
  mkMintingPolicy,
 )

import Plutarch (Config (Config), TracingMode (NoTracing))
import Plutarch.Prelude ()

import PlutusLedgerApi.V1.Scripts (MintingPolicy)

import Plutarch.Api.V1.AssocMap (pdifference, plookup)
import Plutarch.Api.V1.Value (PCurrencySymbol, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC, ptryFromC)
import ThriftFinance.Common.Constants (tfActiveTokenName, tfDepositTokenName, trivialTokenName)
import ThriftFinance.Plutarch.Bool (pall')
import ThriftFinance.Plutarch.Patterns (pfield0, pfield1)
import ThriftFinance.Plutarch.Scripts.V1 (wrapMintingPolicy)
import ThriftFinance.Plutarch.TermCont (pconstantC)
import ThriftFinance.Plutarch.Value (passertSingleAssetFor, phasAnyOfCurrency)
import ThriftFinance.Script.Types (PDeposits, PTFDatum (PTFDatum))
import ThriftFinance.Types (PDepositState (PActiveDepositState, PPendingDepositState), PTFRedeemer (PActivate, PDeposit, PInitialise, PReturn), PTargetProtocolInfo (PTargetProtocolInfo))
import ThriftFinance.Utils (PTFDatumFields, TPInfoF, TPInfoFields, TxInfoF, TxInfoFields)

{- | FIXME: Add Winner selection validation
 The TF minting policy generally follows this strategy to validate all the
 endpoints:
 1. Validate for each TFRedeemer (e.g: "Deposit"), that the
    corresponding TFTokens in the pool input/output are
    minted/burnt
 2. For each TFRedeemer, validate the datum transition
 3. Use the validated datum and other information to validate the new value
    of the pool.
-}
pthriftFinanceToken :: forall (s :: S). Term s (PTFRedeemer :--> PScriptContext :--> PUnit)
pthriftFinanceToken = plam $ \red ctx -> unTermCont $ do
  -- Get fields
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  txInfoF <- pletFieldsC @TxInfoFields ctxF.txInfo
  -- Get own symbol
  ownCs <- pletC $ getOwnSymbol ctxF.purpose
  -- Validate conditions for each redeemer
  pure . pmatch red $ \case
    PInitialise _ -> validateInitialisation txInfoF ownCs
    PDeposit _ -> validateDeposit txInfoF ownCs
    PActivate _ -> validateActivation txInfoF ownCs
    PReturn _ -> validateReturn txInfoF ownCs

{- | In the initialisation, we validate that
 1. An output is created with a `Deposit` TFToken
 2. The output contains a correctly initialised datum and a deposit
 3. No input with a TFToken is consumed
 4. Fees have been paid
-}
validateInitialisation ::
  forall (s :: S).
  TxInfoF s ->
  Term s PCurrencySymbol ->
  Term s PUnit
validateInitialisation txInfoF ownCs = unTermCont $ do
  -- Get pool's continuing output and its datum
  tfOutputF <- pletFieldsC @'["address", "value", "datumHash"] $ getTFOutput # ownCs # txInfoF.outputs
  tfOutputDatum <- pletC $ getTFDatum # txInfoF.datums # tfOutputF.datumHash
  tfOutputDatumF <-
    pletFieldsC @'["targetProtocol", "depositState", "deposits"] $ pto tfOutputDatum
  signer <- getSignatory txInfoF
  (tfTn, tfTokenCount) <- getTFToken ownCs tfOutputF.value
  -- Checks
  pguardC "(validateInitialisation) TFToken should be \"Deposit\"" $
    pconstant tfDepositTokenName #== tfTn
  pguardC "(validateInitialisation) There should only be one \"Deposit\" token" $
    tfTokenCount #== 1
  pguardC
    "(validateInitialisation) initialisation TX may not consume a TF\
    \input"
    $ checkNoTFToken # ownCs # inputValue # txInfoF.inputs
  pguardC "(validateInitialisation) failed to validate initial datum" $
    isInitDatum signer tfOutputDatum
  pguardC "(validateInitialisation) incorrect value for pending deposit" $
    validatePendingDeposit
      tfOutputF.value
      tfOutputDatumF.targetProtocol
      tfOutputDatumF.deposits
  pguardC "(validateInitialisation) error when validating fee payment" $
    validateFees txInfoF tfOutputDatumF.targetProtocol
  pconstantC ()

{- | In a deposit, we validate that:
   1. An output is created with one more `Deposit` TFToken
   2. The output contains a correct datum and deposit
   3. Fees have been paid
-}
validateDeposit ::
  forall (s :: S).
  TxInfoF s ->
  Term s PCurrencySymbol ->
  Term s PUnit
validateDeposit txInfoF ownCs = unTermCont $ do
  -- Get pool's continuing output and its datum
  tfOutputF <- pletFieldsC @'["address", "value", "datumHash"] $ getTFOutput # ownCs # txInfoF.outputs
  tfOutputDatum <- pletC $ getTFDatum # txInfoF.datums # tfOutputF.datumHash
  (newTfTn, newTfTokenCount) <- getTFToken ownCs tfOutputF.value
  tfOutputDatumF <- pletFieldsC @PTFDatumFields $ pto tfOutputDatum
  -- Get pool input and its datum
  tfInputF <-
    pletFieldsC @'["address", "value", "datumHash"] $
      pfield @"resolved"
        # (getTFInput # ownCs # txInfoF.inputs)
  tfInputDatum <- pletC $ getTFDatum # txInfoF.datums # tfInputF.datumHash
  (tfTn, tfTokenCount) <- getTFToken ownCs tfInputF.value
  -- Validate TokenName and count
  pguardC "(validateDeposit) new TFToken should be \"Deposit\"" $
    pconstant tfDepositTokenName #== newTfTn
  pguardC "(validateDeposit) old TFToken should be \"Deposit\"" $
    pconstant tfDepositTokenName #== tfTn
  pguardC "(validateDeposit) There should be *one* more \"Deposit\" token" $
    newTfTokenCount #== tfTokenCount + 1
  -- Get signatory, contributions and datums. Validate datum transition
  signer <- getSignatory txInfoF
  pguardC "(validateDeposit) bad datum transition" $
    isDatumCorrect
      tfInputDatum
      tfOutputDatum
      pendingToPending
      (newContributionFrom # signer)
  -- Validate value in the pool
  let tpInfo = tfOutputDatumF.targetProtocol
  pguardC "(validateDeposit) incorrect value for pending deposit" $
    validatePendingDeposit tfOutputF.value tpInfo tfOutputDatumF.deposits
  -- Validate fee payment
  pguardC "(validateDeposit) error when validating fee payment" $
    validateFees txInfoF tpInfo
  pconstantC ()
  where
    pendingToPending :: forall (s :: S). Term s (PDepositState :--> PDepositState :--> PBool)
    pendingToPending = phoistAcyclic $
      plam $ \inputState outputState ->
        pmatch inputState $ \case
          PPendingDepositState _ -> pmatch outputState $ \case
            PPendingDepositState _ -> pconstant True
            _ -> ptrace "(pendingToPending) output state should be pending" $ pconstant False
          _ -> ptrace "(pendingToPending) input state should be pending" $ pconstant False
    newContributionFrom :: forall (s :: S). Term s (PPubKeyHash :--> PDeposits :--> PDeposits :--> PBool)
    newContributionFrom = phoistAcyclic $
      plam $ \pkh (pto -> inputMap) (pto -> outputMap) -> unTermCont $ do
        -- We check if there already is a contribution from the same user
        pure $
          pmatch (plookup # pkh # inputMap) $ \case
            -- If there is one entry already present, the same entry in the output
            -- should have its contributions increased by one
            PJust oldEntry -> pmatch (plookup # pkh # outputMap) $ \case
              PJust newEntry -> unTermCont $ do
                (stakeCred, n) <- getPair oldEntry
                (stakeCred', n') <- getPair newEntry
                pure $
                  pall'
                    [ ptraceIfFalse "(newContributionFrom) staking credentials were changed" $
                        stakeCred' #== stakeCred
                    , ptraceIfFalse "(newContributionFrom) contributions not increased correctly" $
                        n' #== n + 1
                    ]
              PNothing ->
                ptrace "(newContributionFrom) no contribution from user found in output" $
                  pconstant False
            -- If not present, the difference in the maps should consist of
            -- only* this user's entry. The entry should have *one* contribution.
            PNothing -> pmatch (pto $ pdifference # outputMap # inputMap) $ \case
              PCons newEntry rest ->
                pif
                  (plength # rest #== 0)
                  (ptraceIfFalse "(newContributionFrom) new entry not valid" $ validateEntry pkh newEntry)
                  (ptrace "(newContributionFrom)  expected only one new entry" $ pconstant True)
              PNil ->
                ptrace "(newContributionFrom) no change in the output map" $
                  pconstant False
    validateEntry :: forall (s :: S). Term s PPubKeyHash -> Term s (PBuiltinPair (PAsData PPubKeyHash) (PAsData (PBuiltinPair (PAsData (PMaybeData PStakingCredential)) (PAsData PInteger)))) -> Term s PBool
    validateEntry pkh pair = unTermCont $ do
      (pkh', valuePair) <- getPair pair
      (_stakingCred, n) <- getPair valuePair
      pure $
        pall'
          [ ptraceIfFalse "(newContributionFrom) new entry does not belong to user" $
              pkh #== pkh'
          , ptraceIfFalse "(newContributionFrom) new entry should have *one* contribution" $
              n #== 1
          ]

{- | When activating a pool, we validate that:
   1. An output is created with one `Active` TFToken
   2. The input contains as many `Deposit` tokens as the target number of
      contributions
   3. The output contains a correct datum and deposit
   4. The target value of the pool is deposited into the target protocol
      contract
   5. Fees have been paid
-}
validateActivation ::
  forall (s :: S).
  TxInfoF s ->
  Term s PCurrencySymbol ->
  Term s PUnit
validateActivation txInfoF ownCs = unTermCont $ do
  -- Get pool's continuing output and its datum
  tfOutputF <- pletFieldsC @'["address", "value", "datumHash"] $ getTFOutput # ownCs # txInfoF.outputs
  tfOutputDatum <- pletC $ getTFDatum # txInfoF.datums # tfOutputF.datumHash
  (newTfTn, newTfTokenCount) <- getTFToken ownCs tfOutputF.value
  -- Get pool input and its datum
  tfInputF <-
    pletFieldsC @'["address", "value", "datumHash"] $
      pfield @"resolved"
        # (getTFInput # ownCs # txInfoF.inputs)
  tfInputDatum <- pletC $ getTFDatum # txInfoF.datums # tfInputF.datumHash
  (tfTn, tfTokenCount) <- getTFToken ownCs tfInputF.value
  -- Validate TF TokenNames and counts
  PTFDatum newDat <- pmatchC tfOutputDatum
  let nTargetContributions :: Term s PInteger
      nTargetContributions =
        pfield @"numberOfContributions" #$ pto $ pfromData $ pfield @"targetProtocol" # newDat
  pguardC "(validateActivation) new TFToken should be \"Active\"" $
    pconstant tfActiveTokenName #== newTfTn
  pguardC "(validateActivation) there should be only one \"Active\" token" $
    newTfTokenCount #== 1
  pguardC "(validateActivation) old TFToken should be \"Deposit\"" $
    pconstant tfDepositTokenName #== tfTn
  pguardC
    "(validateActivation) there should be as many \"Deposit\" tokens in the\
    \ input as the target number of contributions"
    $ tfTokenCount #== nTargetContributions
  -- Validate datum
  pguardC "(validateActivation) bad datum transition" $
    isDatumCorrect
      tfInputDatum
      tfOutputDatum
      datumMadeActive
      (plam (#==))
  -- Get pool parameters
  PTFDatum newDat <- pmatchC tfOutputDatum
  tpInfo <- pletC . pfromData $ pfield @"targetProtocol" # newDat
  tpInfoF <-
    pmatchC tpInfo >>= \(PTargetProtocolInfo i) ->
      pletFieldsC
        @'[ "depositTarget"
          , "passiveDepositCurrency"
          , "passiveDepositName"
          , "activeDepositCurrency"
          , "lockingScript"
          ]
        i
  -- Validate deposit of target token in pool
  pguardC "(validateActivation) incorrect amount of target tokens deposited in pool" $
    let poolTpTokens =
          pvalueOf
            # tfOutputF.value
            # tpInfoF.activeDepositCurrency
            # pconstant trivialTokenName
     in tpInfoF.depositTarget #<= poolTpTokens
  -- Validate deposit of collected pool funds in target protocol
  tpOutputF <-
    pletFieldsC @'["value"] $
      (getTPOutput # pfromData tpInfoF.lockingScript # pfromData txInfoF.outputs)
  pguardC
    "(validateActivation) incorrect amount of pending deposit currency\
    \ in locking script"
    $ let targetPoolTokens =
            pvalueOf
              # tpOutputF.value
              # tpInfoF.passiveDepositCurrency
              # tpInfoF.passiveDepositName
       in pfromData tpInfoF.depositTarget #<= targetPoolTokens
  -- Validate fee payment
  pguardC "(validateActivation) error when validating fee payment" $
    validateFees txInfoF tpInfo
  pconstantC ()
  where
    datumMadeActive :: forall (s :: S). Term s (PDepositState :--> PDepositState :--> PBool)
    datumMadeActive = phoistAcyclic $
      plam $ \inputState outputState ->
        pmatch inputState $ \case
          PPendingDepositState _ -> pmatch outputState $ \case
            PActiveDepositState _ -> pconstant True
            _ ->
              ptrace "(datumMadeActive) expected an output with an active deposit state" $
                pconstant False
          _ ->
            ptrace "(datumMadeActive) expected an input with a pending deposit state" $
              pconstant False

{- | When returning, we validate that
 1. Both the TF pool and the TP pool are consumed
 2. The TF pool contains the correct TF token
 3. No output is created with the TF or TP address
 4. All the users get their original contributions back
 5. The randomly selected winner gets her rewards
 6. Fees have been paid
-}
validateReturn ::
  forall (s :: S).
  TxInfoF s ->
  Term s PCurrencySymbol ->
  Term s PUnit
validateReturn txInfoF ownCs = unTermCont $ do
  -- Get TF pool input and its datum
  tfInputF <-
    pletFieldsC @'["address", "value", "datumHash"] $
      pfield @"resolved"
        # (getTFInput # ownCs # txInfoF.inputs)
  tfInputDatum <- pletC $ getTFDatum # txInfoF.datums # tfInputF.datumHash
  tfInputDatumF <- pletFieldsC @PTFDatumFields $ pto tfInputDatum
  (tfTn, tfTokenCount) <- getTFToken ownCs tfInputF.value
  -- Get pool parameters
  let tpInfo = tfInputDatumF.targetProtocol
  tpInfoF <- pletFieldsC @TPInfoFields $ pto tpInfo
  -- Validate TF TokenNames and counts
  pguardC "(validateReturn) TFToken should be \"Active\"" $
    pconstant tfActiveTokenName #== tfTn
  pguardC "(validateReturn) there should be only one \"Active\" token" $
    tfTokenCount #== 1
  -- Validate that each user gets their original contribution
  pguardC "(validateReturn) failure when validating return of contributions" $
    validateReturnedContributions txInfoF tpInfoF tfInputDatumF.deposits
  pguardC "(validateReturn) error when validating fee payment" $
    validateFees txInfoF tpInfo
  pconstantC ()

{- | This function takes the value of the pool and its (assumed correct) datum.
 If the value matches the information in the datum, it validates correctly.
-}
validatePendingDeposit :: forall (s :: S). Term s (PValue 'Sorted 'Positive) -> Term s PTargetProtocolInfo -> Term s PDeposits -> Term s PBool
validatePendingDeposit val tpInfo deposits = unTermCont $ do
  -- Add up the number of contributions
  nContributions :: Term s PInteger <-
    pletC $
      pfoldl
        # plam (\acc (pfromData . (psndBuiltin #) . pfromData . (psndBuiltin #) -> n) -> acc + n)
        # 0
        # pto (pto deposits)
  -- Get target value and target number of contributions
  tpInfoF <- pletFieldsC @TPInfoFields $ pto tpInfo
  -- For every token in the target value, check that all contributions have been made
  -- pure $
  pure $
    validateValues
      # val
      # tpInfoF.passiveDepositCurrency
      # tpInfoF.passiveDepositName
      # tpInfoF.depositTarget
      # nContributions
      # tpInfoF.numberOfContributions

{- | This function validates that all contributors get their deposits back.
 To do that, it checks for each output whether there is a contribution with
 that PKH and validates the amount that is owed and should be returned.
-}
validateReturnedContributions ::
  forall (s :: S).
  TxInfoF s ->
  TPInfoF s ->
  Term s PDeposits ->
  Term s PBool
validateReturnedContributions txInfoF tpInfoF contributions = unTermCont $ do
  let contributorsPaid :: Term s PInteger
      contributorsPaid =
        pfoldl # (go # contributions)
          # 0
          # pfromData txInfoF.outputs
      go :: Term s (PDeposits :--> PInteger :--> PTxOut :--> PInteger)
      go = plam $ \(pto -> contribs) acc txOut -> unTermCont $ do
        txOutF <- pletFieldsC @'["value", "address"] txOut
        addressF <- pletFieldsC @'["credential", "stakingCredential"] txOutF.address
        pure $
          pmatch addressF.credential $ \case
            PPubKeyCredential (pfromData . pfield0 -> pkh) ->
              pmatch (plookup # pkh # contribs) $ \case
                -- Contributor found
                PJust pair -> unTermCont $ do
                  (stakingCredential', n) <- getPair pair
                  -- Check that value deposited matches number of contributions and
                  -- that staking cred is preserved.
                  pure $
                    pif
                      ( validateReturn tpInfoF txOutF.value n
                          #&& validateStakeCred addressF.stakingCredential stakingCredential'
                      )
                      (acc + 1)
                      acc
                -- Not a contributor
                PNothing -> acc
            _ -> acc
      validateReturn :: TPInfoF s -> Term s (PValue 'Sorted 'Positive) -> Term s PInteger -> Term s PBool
      validateReturn tpInfoF val n = unTermCont $ do
        let err =
              "Value: " <> pshow (pvalueOf # val # tpInfoF.passiveDepositCurrency # tpInfoF.passiveDepositName) <> "\n"
                <> "N: "
                <> pshow n
                <> "\n"
                <> "Target: "
                <> pshow (pdiv # (n * tpInfoF.depositTarget) # tpInfoF.numberOfContributions)
                <> "\n"
        pure $
          ptraceIfFalse ("(validateReturnedContributions) returned value does not match number of contributions" <> err) $
            pdiv # (n * tpInfoF.depositTarget) # tpInfoF.numberOfContributions
              #<= pvalueOf # val # tpInfoF.passiveDepositCurrency # tpInfoF.passiveDepositName
      validateStakeCred :: Term s (PMaybeData PStakingCredential) -> Term s (PMaybeData PStakingCredential) -> Term s PBool
      validateStakeCred c = ptraceIfFalse "(validateReturnedContributions) staking credential altered" . (c #==)

  pure $
    ptraceIfFalse "(validateReturnedContributions) not all contributors had their deposits back" $
      unTermCont $ do
        _ <- ptraceC $ "[contributorsPaid] " <> pshow contributorsPaid
        pure $ contributorsPaid #== plength # pto (pto contributions)

{- | For every asset in the target value, it checks that the same asset
 is present in the pool value *and* that its quantity is at least
 the expected value given the number of contributions made
-}
validateValues ::
  forall (s :: S).
  Term
    s
    ( PValue 'Sorted 'Positive
        -- Value in the pool
        :--> PCurrencySymbol
        -- Target deposit's currency symbol
        :--> PTokenName
        -- Target deposit's token name
        :--> PInteger
        -- Target deposit's count
        :--> PInteger
        -- Number of contributions made
        :--> PInteger
        -- Target number of contributions
        :--> PBool
    )
validateValues = phoistAcyclic $
  plam $ \val targetCs targetTn targetN nContributions nTargetContributions ->
    let poolN = pvalueOf # val # targetCs # targetTn
     in targetN #<= (pdiv # (poolN * nTargetContributions) # nContributions)

{- | Verify that an output was created with the fee value and address
 defined in `TargetProtocolInfo`
-}
validateFees ::
  forall (s :: S).
  TxInfoF s ->
  Term s PTargetProtocolInfo ->
  Term s PBool
validateFees txInfoF tpInfo = unTermCont $ do
  -- Get fees and fee recipient
  tpInfoF <-
    pmatchC tpInfo >>= \(PTargetProtocolInfo i) ->
      pletFieldsC @'["fee", "feeRecipient"] i
  let fee = PValue.passertPositive #$ PValue.passertSorted # tpInfoF.fee
  -- We find the output with the fee recipient's address
  let candidateOutputs =
        pfilter
          # (hasAddress # pfromData tpInfoF.feeRecipient)
          # pfromData txInfoF.outputs
  pure $
    pmatch candidateOutputs $ \case
      PCons out rest ->
        pif
          (plength # rest #== 0)
          ( ptraceIfFalse "(validateFees) fees paid are insufficient" $
              fee #<= pfield @"value" # out
          )
          ( ptrace
              "(validateFees) expected to find only one output\
              \ for fee recipient"
              $ pconstant False
          )
      PNil -> pconstant False

-- | Thrift Finance Token Minting Policy - Plutarch term.
pthriftFinanceTokenMp :: ClosedTerm PMintingPolicy
pthriftFinanceTokenMp = wrapMintingPolicy pthriftFinanceToken

-- |  Thrift Finance Token Minting Policy ~ no tracing.
thriftFinanceTokenMP :: MintingPolicy
thriftFinanceTokenMP = mkMintingPolicy (Config NoTracing) pthriftFinanceTokenMp

------ HELPERS ------

-- | Fetches the address of a `PTxOut` and compares it to a given `PAddress`.
hasAddress :: forall (s :: S). Term s (PAddress :--> PTxOut :--> PBool)
hasAddress = phoistAcyclic $ plam $ \addr -> (#== addr) . (pfield @"address" #)

-- | Fetches the `PCurrencySymbol` of a minting policy
getOwnSymbol :: forall (s :: S). Term s PScriptPurpose -> Term s PCurrencySymbol
getOwnSymbol = flip pmatch $ \case
  PMinting (pfield0 -> cs) -> cs
  _ -> ptraceError "(getOwnSymbol) not a minting policy"

{- | Fetches the TF output. This should be the only output with a TF Token.
 If this condition is not met, the function fails
-}
getTFOutput :: forall (s :: S). Term s (PCurrencySymbol :--> PBuiltinList PTxOut :--> PTxOut)
getTFOutput = phoistAcyclic $ plam $ \cs -> getWithTFToken # cs # outputValue

{- | Fetches the TF input. This should be the only input with a TF Token.
 If this condition is not met, the function fails
-}
getTFInput :: forall (s :: S). Term s (PCurrencySymbol :--> PBuiltinList PTxInInfo :--> PTxInInfo)
getTFInput = phoistAcyclic $ plam $ \cs -> getWithTFToken # cs # inputValue

{- | Fetches the TP output. There should only be one output with this address.
 Otherwise, the function fails.
-}
getTPOutput :: forall (s :: S). Term s (PAddress :--> PBuiltinList PTxOut :--> PTxOut)
getTPOutput = phoistAcyclic $
  plam $ \addr ->
    getWithAddress # addr # plam (pfield @"address" #)

{- | Fetches the TP output. There should only be one output with this address.
 Otherwise, the function fails.
-}
_getTPInput :: forall (s :: S). Term s (PAddress :--> PBuiltinList PTxInInfo :--> PTxInInfo)
_getTPInput = phoistAcyclic $
  plam $ \addr ->
    getWithAddress # addr #$ plam ((pfield @"address" #) . (pfield @"resolved" #))

{- | Helper for getting an input/output that contains a TFToken.
 If zero/multiple elements are founds, it fails.
-}
getWithTFToken ::
  forall (s :: S) (a :: PType).
  PLift a =>
  Term
    s
    ( -- Own currency symbol
      PCurrencySymbol
        :-->
        -- Function for getting a Value from the element
        (a :--> PValue 'Sorted 'Positive)
        :-->
        -- List of elements to search on
        PBuiltinList a
        :--> a
    )
getWithTFToken = phoistAcyclic $
  plam $ \cs getValue ls ->
    pelimList
      ( pelimList
          (\_ _ -> ptraceError "(getWithTFToken) more than one input/output with TF token found")
      )
      (ptraceError "(getWithTFToken) no input/output with TF token found")
      (pfilter # (phasCs # cs # getValue) # ls)

{- | Helper for getting an input/output with a given address.
 If zero/multiple elements are founds, it fails.
-}
getWithAddress ::
  forall (s :: S) (a :: PType).
  PLift a =>
  Term
    s
    ( -- Address to search with
      PAddress
        :-->
        -- Function for getting an Address from the element
        (a :--> PAddress)
        :-->
        -- List of elements to search on
        PBuiltinList a
        :--> a
    )
getWithAddress = phoistAcyclic $
  plam $ \addr getAddr ls ->
    pelimList
      ( pelimList
          (\_ _ -> ptraceError "(getWithAddress) more than one input/output with given address")
      )
      (ptraceError "(getWithAddress) no input/output with given address")
      (pfilter # (phasAddr # addr # getAddr) # ls)

{- | Obtains the `PTokenName` and count of the asset with the given
 `PCurrencySymbol`. If more than one asset with the given CS is found,
 it fails.
-}
getTFToken ::
  forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
  -- | Own currency symbol
  Term s PCurrencySymbol ->
  -- | Value to inspect
  Term s (PValue keys amounts) ->
  TermCont s (Term s PTokenName, Term s PInteger)
getTFToken cs val =
  getPair $
    passertSingleAssetFor
      "(getTFToken) could not find only 1 token with given currency symbol"
      # cs
      # val

-- | Obtains the number of tokens of the target protocol in the given value.
_getTPTokens ::
  forall (s :: S).
  Term s PTargetProtocolInfo ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s PInteger
_getTPTokens tpInfo val = unTermCont $ do
  PTargetProtocolInfo info <- pmatchC tpInfo
  let targetCs :: Term s PCurrencySymbol
      targetCs = pfield @"activeDepositCurrency" # info
  pure $ pvalueOf # val # targetCs # pconstant trivialTokenName

-- | Helper that checks an input/output list does *not* contain an input/output with a TF token
checkNoTFToken ::
  forall (s :: S) (a :: PType).
  PLift a =>
  Term
    s
    ( PCurrencySymbol
        :--> (a :--> PValue 'Sorted 'Positive)
        :--> PBuiltinList a
        :--> PBool
    )
checkNoTFToken = phoistAcyclic $
  plam $ \cs getValue ls -> unTermCont $ do
    pure $
      pelimList
        (\_ _ -> pconstant False)
        (pconstant True)
        (pfilter # (phasCs # cs # getValue) # ls)

{- | It takes the user's address and the datum of the pool and validates that
 it's the correct initial datum
-}
isInitDatum :: forall (s :: S). Term s PPubKeyHash -> Term s PTFDatum -> Term s PBool
isInitDatum pkh dat' = unTermCont $ do
  PTFDatum dat <- pmatchC dat'
  datF <- pletFieldsC @'["targetProtocol", "depositState", "deposits"] dat
  -- Deposit state must be Pending
  pure . pmatch datF.depositState $ \case
    PPendingDepositState _ -> unTermCont $ do
      -- There must be only one contribution
      pure . pmatch (pto $ pto $ pfromData datF.deposits) $ \case
        PCons pair rest -> unTermCont $ do
          (pkh', _) <- getPair pair
          pure . pall' $
            [ ptraceIfFalse
                "(isInitDatum) address not in initial\
                \ contributions list"
                $ pkh' #== pkh
            , ptraceIfFalse
                "(isInitDatum) initial contributions list\
                \ should not have have more than one element"
                $ plength # rest #== 0
            ]
        PNil ->
          ptrace
            "(isInitDatum) initial contributions map\
            \ should not be empty"
            $ pconstant False
    _ ->
      ptrace
        "(isInitDatum) initial datum of the pool must have pending\
        \ state"
        $ pconstant False

{- | Helper for validating a datum transition. It always checks that
 the `PTargetProtocolInfo` field is untouched, while taking a function that
 validates the `PDepositState` field.
-}
isDatumCorrect ::
  forall (s :: S).
  -- | Previous datum
  Term s PTFDatum ->
  -- | New datum
  Term s PTFDatum ->
  -- | Function validating any change in the deposit state
  Term s (PDepositState :--> PDepositState :--> PBool) ->
  -- | Function validating any change in the contribution map
  Term s (PDeposits :--> PDeposits :--> PBool) ->
  Term s PBool
isDatumCorrect prevDat newDat validateState validateDeposits = unTermCont $ do
  prevDatF <- pletFieldsC @PTFDatumFields $ pto prevDat
  newDatF <- pletFieldsC @PTFDatumFields $ pto newDat
  pure . pall' $
    [ ptraceIfFalse "(isDatumCorrect) targetProtocol altered" $
        prevDatF.targetProtocol #== newDatF.targetProtocol
    , ptraceIfFalse "(isDatumCorrect) conditions on depositState not satisfied" $
        validateState # prevDatF.depositState # newDatF.depositState
    , ptraceIfFalse "(isDatumCorrect) conditions on deposits not satisfied" $
        validateDeposits # prevDatF.deposits # newDatF.deposits
    ]

-- | Parses the datum from a TF UTxO as a TFDatum.
getTFDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData (PTuple PDatumHash PDatum))
        :--> PMaybeData PDatumHash
        :--> PTFDatum
    )
getTFDatum = phoistAcyclic $
  plam $ \datums datHash' -> unTermCont $ do
    -- Check if TxOut contains a datum hash
    pure . pmatch datHash' $ \case
      PDJust (pfromData . pfield0 -> datHash) ->
        let tup' = pfind # (matchesHash # datHash) # datums
         in -- Check if a datum with given hash was found
            pmatch tup' $ \case
              PJust (pfromData . pfield1 . pfromData -> dat) -> unTermCont $ do
                PDatum d <- pmatchC dat
                fst <$> ptryFromC @PTFDatum d
              _ -> ptraceError "(getTFDatum) no datum found with given hash"
      _ -> ptraceError "(getTFDatum) TxOut does not contain datum hash"
  where
    matchesHash :: forall (s :: S). Term s (PDatumHash :--> PAsData (PTuple PDatumHash PDatum) :--> PBool)
    matchesHash = phoistAcyclic $
      plam $ \hash (pfromData -> tup) ->
        let hash' = pfromData . pfield0 $ tup
         in hash #== hash'

-- | Gets the *single* signatory of the transaction
getSignatory :: forall (s :: S). TxInfoF s -> TermCont s (Term s PPubKeyHash)
getSignatory txInfoF = do
  let signatories :: Term s (PBuiltinList (PAsData PPubKeyHash)) = pfromData txInfoF.signatories
  pure $
    pmatch signatories $ \case
      PCons (pfromData -> pkh) rest -> unTermCont $ do
        pguardC "(getSignatory) only one user may sign the TX" $
          plength # rest #== 0
        pure pkh
      PNil -> ptraceError "(getSignatory) no signatory found"

-- | Helper for getting a value from an input/output and seeing if it contains a TFToken
phasCs :: Term s (PCurrencySymbol :--> (a :--> PValue 'Sorted 'Positive) :--> (a :--> PBool))
phasCs = phoistAcyclic $
  plam $ \cs getValue a ->
    phasAnyOfCurrency @PList # (pcons # pdata cs # pnil) #$ getValue # a

-- | Helper for getting an address from an input/output and seeing if it is equal to the given one
phasAddr :: Term s (PAddress :--> (a :--> PAddress) :--> (a :--> PBool))
phasAddr = phoistAcyclic $ plam $ \addr getAddr -> (addr #==) . (getAddr #)

-- | Helper for getting a value from a `PTxInInfo`
inputValue :: forall (s :: S). Term s (PTxInInfo :--> PValue 'Sorted 'Positive)
inputValue = phoistAcyclic $ plam $ (outputValue #) . (pfield @"resolved" #)

-- | Helper for getting a value from a `PTxOut`
outputValue :: forall (s :: S). Term s (PTxOut :--> PValue 'Sorted 'Positive)
outputValue = phoistAcyclic $ plam (pfield @"value" #)

-- | Helper for getting the elements inside a pair
getPair ::
  forall (s :: S) (a :: PType) (b :: PType).
  (PIsData a, PIsData b) =>
  Term s (PBuiltinPair (PAsData a) (PAsData b)) ->
  TermCont s (Term s a, Term s b)
getPair p = pure (pfromData $ pfstBuiltin # p, pfromData $ psndBuiltin # p)
