# Components and types

## Description
 
The document describes a high-level description of the types and components
involved in the Thrift Finance protocol - serving as a guide and aid in the
implementation of the protocol. The document should be considered a stable SSoT
with regards to the protocol.

## Types
  
The following components are defined:

```agda
-- as I am using this to sketch out the types it shouldn't be relevant - beware it doesn't TC otherwise.
{-# OPTIONS --type-in-type #-} 

-- As we're not describing implementation - I'll postulate for now the following
postulate
  ByteString       : Set
  Integer          : Set
  List             : Set → Set
  ScriptContext    : Set
  Slot             : Integer → Set
  Value            : Set
  Wallet           : Set
  
data ValidationResult : Set  where
  Pass : ValidationResult
  Fail : ValidationResult

_&_ : ValidationResult → ValidationResult → ValidationResult
Pass & Pass = Pass
Pass & Fail = Fail
Fail & _    = Fail

data ScriptType : Set where
  MintingPolicy : ScriptType
  Validator     : ScriptType

data CompiledScript : ScriptType → Set where
  CompiledMintingPolicy : (redeemer : Set) → (redeemer → ScriptContext → ValidationResult) → CompiledScript MintingPolicy
  CompiledValidator     : (datum : Set) → (redeemer : Set) → (datum → redeemer → ScriptContext → ValidationResult) → CompiledScript Validator

data CurrencySymbol : CompiledScript MintingPolicy → Set where
  getCurrencySymbol : (mintingPolicy : CompiledScript MintingPolicy) → CurrencySymbol mintingPolicy

data ScriptAddress : CompiledScript Validator → Set where
  scriptAddress : (validator : CompiledScript Validator) → ScriptAddress validator

data WalletAddress : Wallet → Set where
  walletAddress : (wallet : Wallet) → WalletAddress wallet

data TokenName : Set where
  tokenName : ByteString → TokenName

data Asset : Set where
  mkAsset : {mintingPolicy : CompiledScript MintingPolicy} → CurrencySymbol mintingPolicy → TokenName → Asset
```

## Thrift Finance Datum

The Thrift Finance Datum indicates the scope of the locked funds. There are
three types of locked funds: Pending Deposits, Active Deposits, and Returns. Let
us describe what goes into these datums.

The target protocol information contains all the information regarding to what
will happen with the submitted information.

```agda
record TargetProtocolInformation : Set where
  constructor targetProtocolInformation

  field LockingScript          : { validator : CompiledScript Validator } → ScriptAddress validator
    -- ^ The locking script is indicative of where the funds will be locked
    -- after reaching the contribution/contributors threshold and activating the deposit

  field DepositTarget          : Value
    -- ^ Deposit target is the total amount of funds expected to be raised
    -- before the deposit is triggered.

  field NumberOfContributions  : Integer
    -- ^ Deposit indicates how many contributions need to be made to reach the
    -- target. It should be chosen such that the deposit target is divisible by
    -- the amount of expected Number of Contributions.

  field ActiveDepositCurrency : ∀ arbitraryCoin → CurrencySymbol arbitraryCoin
   -- ^ The expected currency symbol of the return of the depositing action.

  field ClaimReturnAfter      : Integer
  -- ^ Indicates how long after the triggering of the deposit the Return
  -- transaction can be triggered.

{- For now we've decided that tokens/lending pools don't require to be
  distinguishable for the same TargetProtocol given the same information ~
  simpler - TO BE REVIEWED further down the line.

  field PoolExpires : ∀ n → Slot n
  --^ The pool aims to collect the DepositTarget until this specific slot.
-}
```

A Pending Transaction's datum contains information about the wallets that will
receive the reward, and which TargetProtocol it is aiming for. In essence the
datum is representative of many contributors signing up to participate in the
lending pool.

```agda
record PendingDepositInformation : Set where
  constructor pendingDepositInformation

  field TargetProtocol  : TargetProtocolInformation
  -- ^ TargetProtocol information.

  field Contributions   : List ({ submitter : Wallet } → WalletAddress submitter)
  -- ^ The return addresses indicates what wallets have already contributed to
  -- the EUTxO.
```

An Active Deposit datum holds the information relating to a previously executed
transaction. It is a witness of an executed transaction. Its associated EUTxO
contains all the tokens resulting from the Protocol's  transaction.

```agda
record ActiveDepositInformation : Set where
  constructor activeDepositInformation

  field TargetProtocol : TargetProtocolInformation
  -- ^ TargetProtocol information.

  field Contributions  : List ({ contributorWallet : Wallet } → WalletAddress contributorWallet)
  -- ^ List of all the addresses of the included Contribution. Some wallets
  -- might appear more than once - based on how many contributions they made.

  field SubmittedAt    : ∀ submissionTime → Slot submissionTime
  -- ^ Slot number in which the transaction was made.
```


Thus we can define the following:

```agda
data TFDatum : Set where
  PendingDepositDatum : PendingDepositInformation → TFDatum
  ActiveDepositDatum  : ActiveDepositInformation  → TFDatum
```

## Thrift Finance Minting Policy

The Thrift Finance Minting Policy is a pre-defined Minting Policy that verifies
the correct format of the datum of the eutxo it is placed in. The thrift-finance
tokens appear on pools at the moment of activating a deposit and disapear at the
moment of all the returns being claimed. 

```agda
data TFRedeemer : Set where
  InitialisePolicy    : TFRedeemer
  ActivatePolicy      : TFRedeemer
  ClaimReturn         : TFRedeemer

TFMintingPolicy : CompiledScript MintingPolicy
TFMintingPolicy = CompiledMintingPolicy TFRedeemer TFMintingPolicyLogic
  where
    postulate continuingOutputDatumIsCorrect : ScriptContext -> ValidationResult
    -- ^ The target protocol information is maintained across continuing
    -- outputs.
  
    postulate correctDatumFormatOutput : {a : Set} → (datumType : a → TFDatum) → ScriptContext → ValidationResult
    -- ^ The minting policy checks that the datum in which the eUTxO is
    -- correctly formulated.

    postulate targetIsDivisibleByNumberOfContributions : ScriptContext → ValidationResult
    -- ^ For a deposit to be correct the target must be divisible by the
    -- number of contributors.
    
    postulate datumTransitionIsCorrect : {a b : Set} → (datumType : a → TFDatum) → (datumType : b → TFDatum) → ScriptContext → ValidationResult

    postulate tokenCreationIsCorrect : {a : Set} → (datumType : a → TFDatum) → ScriptContext → ValidationResult
    -- ^ Checks that the token is correctly minted given a datum.

    postulate tokenBurningIsCorrect : {a : Set} → (datumType : a → TFDatum) → ScriptContext → ValidationResult
    -- ^ Checks that the token is correctly minted given a datum.


    TFMintingPolicyLogic : TFRedeemer → ScriptContext → ValidationResult

    TFMintingPolicyLogic InitialisePolicy context
      = continuingOutputDatumIsCorrect context
      &  tokenCreationIsCorrect PendingDepositDatum context

    TFMintingPolicyLogic ActivatePolicy context
      = continuingOutputDatumIsCorrect context
      & datumTransitionIsCorrect PendingDepositDatum ActiveDepositDatum context
    
    TFMintingPolicyLogic ClaimReturn context
      = continuingOutputDatumIsCorrect context
      & tokenBurningIsCorrect ActiveDepositDatum context
```

## Thrift Finance Script

The Thrift Finance Address logic verifies the correct handling of EUTxO's locked
under its address. The TF derivies its logic from the datum locked (specifically
the `TargetProtocolInformation`) with the Thrift Finance Minting Policy token.

```agda
TFScript : CompiledScript Validator
TFScript = CompiledValidator TFDatum TFRedeemer (TFAddressLogic TFTKCurrencySymbol)
  where
    postulate TFAddressLogic : CurrencySymbol TFMintingPolicy → TFDatum → TFRedeemer → ScriptContext → ValidationResult
    TFTKCurrencySymbol = getCurrencySymbol TFMintingPolicy
```

The Thrifth Finance Address is shared by all the Peer Pools.

```agda
TFAddress : ScriptAddress TFScript
TFAddress = scriptAddress TFScript
```
