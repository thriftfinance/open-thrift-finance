module Main (main) where

import Plutarch (Config (Config, tracingMode), TracingMode (NoTracing))
import Ply.Plutarch (writeTypedScript)

import ThriftFinance (
  otherConstTrueScript,
  pconstTrueValidator,
  pthriftFinanceTokenMp,
  pthriftFinanceValidator,
  trivialRewardTkM,
  trivialTkM,
 )

import ThriftFinance.Common.Constants (
  constTrueValidatorFile,
  logWriting,
  otherConstTrueValidatorFile,
  refreshDirectory,
  scriptOutDirOnchain,
  tfTokenFile,
  tfValidatorFile,
  trivialRewardTokenFile,
  trivialTokenFile,
 )

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)

{- | Function that compiles all the necessary scripts and saves them locally -
 to be used by our offchain code.
-}
main :: IO ()
main = do
  putStrLn "> Starting script compilation."
  flip runReaderT scriptOutDirOnchain $ do
    let tracingMode = Config {tracingMode = NoTracing}

    refreshDirectory

    constTrueValidatorFile' <- constTrueValidatorFile
    logWriting constTrueValidatorFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Const True Validator (DoTracing)"
        constTrueValidatorFile'
        pconstTrueValidator

    otherConstTrueValidatorFile' <- otherConstTrueValidatorFile
    logWriting otherConstTrueValidatorFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Other Const True Validator (DoTracing)"
        otherConstTrueValidatorFile'
        otherConstTrueScript

    trivialTokenFile' <- trivialTokenFile
    logWriting trivialTokenFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Trivial Protocol - Liquidity token's minting policy (DoTracing)"
        trivialTokenFile'
        trivialTkM

    trivialRewardTokenFile' <- trivialRewardTokenFile
    logWriting trivialRewardTokenFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Trivial protocol - Reward token's minting policy (DoTracing)"
        trivialRewardTokenFile'
        trivialRewardTkM

    tfTokenFile' <- tfTokenFile
    logWriting tfTokenFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Thrift Finance Token (DoTracing)"
        tfTokenFile'
        pthriftFinanceTokenMp

    tfValidatorFile' <- tfValidatorFile
    logWriting tfValidatorFile'
    liftIO $
      writeTypedScript
        tracingMode
        "Thrift Finance Validator (DoTracing)"
        tfValidatorFile'
        pthriftFinanceValidator
