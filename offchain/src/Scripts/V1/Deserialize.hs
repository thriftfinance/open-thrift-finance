module Scripts.V1.Deserialize (
  readConstTrueValidator,
  readOtherConstTrueValidator,
  readTrivialTkF,
  readTrivialRewardTkF,
  readThriftTkMp,
  readThriftValidator,
) where

import Data.ByteString.Internal (ByteString)
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, Validator)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  readTypedScript,
  toMintingPolicy,
  toValidator,
  (#),
 )

import Control.Monad.Reader (ReaderT (runReaderT))
import ThriftFinance.Common.Constants (
  FileLocation,
  constTrueValidatorFile,
  otherConstTrueValidatorFile,
  scriptOutDirOffchain,
  tfTokenFile,
  tfValidatorFile,
  trivialRewardTokenFile,
  trivialTokenFile,
 )
import ThriftFinance.Offchain.Utils (validatorHashBs)

-- | Some util.
go :: forall a. FileLocation a -> IO a
go = flip runReaderT scriptOutDirOffchain

-- | Get the Thift Finance minting policy
readThriftTkMp :: IO MintingPolicy
readThriftTkMp = do
  tfTokenFile' <- go tfTokenFile
  toMintingPolicy <$> readTypedScript tfTokenFile'

-- | Get the parameterised Thrift Finance validator
readThriftValidator :: IO (CurrencySymbol -> Validator)
readThriftValidator = do
  tfValidatorFile' <- go tfValidatorFile
  tfValidatorF <- readTypedScript @ValidatorRole @'[CurrencySymbol] tfValidatorFile'
  pure $ toValidator . (tfValidatorF #)

-- | Get the trivial, always-true validator
readConstTrueValidator :: IO Validator
readConstTrueValidator = do
  constTrueValidatorFile' <- go constTrueValidatorFile
  toValidator <$> readTypedScript constTrueValidatorFile'

-- | Get another trivial, always-true validator
readOtherConstTrueValidator :: IO Validator
readOtherConstTrueValidator = do
  otherConstTrueValidatorFile' <- go otherConstTrueValidatorFile
  toValidator <$> readTypedScript otherConstTrueValidatorFile'

{- | Get the parameterised minting policy of the trivial protocols' liquidity
 | token
-}
readTrivialTkF :: IO (Validator -> MintingPolicy)
readTrivialTkF = do
  trivialTokenFile' <- go trivialTokenFile
  trivialTkMp <- readTypedScript @MintingPolicyRole @'[ByteString] $ trivialTokenFile'
  pure $ toMintingPolicy . (trivialTkMp #) . toByteString . validatorHashBs

{- | Get the parameterised minting policy of the trivial protocols' reward
 | token
-}
readTrivialRewardTkF :: IO (CurrencySymbol -> MintingPolicy)
readTrivialRewardTkF = do
  trivialRewardTokenFile' <- go trivialRewardTokenFile
  trivialRewardTkMp <- readTypedScript @MintingPolicyRole @'[CurrencySymbol] $ trivialRewardTokenFile'
  pure $ toMintingPolicy . (trivialRewardTkMp #)

toByteString :: BuiltinByteString -> ByteString
toByteString (BuiltinByteString bs) = bs
