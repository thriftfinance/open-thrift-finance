{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ThriftFinance.Common.Constants
  ( constTrueValidatorFile
  , logWriting
  , scriptOutDirOffchain
  , scriptOutDirOnchain
  , tfTokenFile
  , tfValidatorFile
  , tfDepositTokenName
  , tfActiveTokenName
  , trivialRewardTokenFile
  , refreshDirectory
  , trivialTokenName
  , trivialRewardTokenName
  , otherConstTrueValidatorFile
  , trivialTokenFile
  , FileLocation
  ) where

import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath  ((</>))
import System.IO.Error  (isAlreadyExistsError)

import Control.Exception (catch, throwIO)
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT,ask)
import Control.Monad.IO.Class (liftIO, MonadIO)

#ifdef NEW_LEDGER_NAMESPACE

import PlutusLedgerApi.V1.Value(TokenName(TokenName))

#else

import Plutus.V1.Ledger.Value (TokenName(TokenName))

#endif

type FileLocation a = forall m . MonadIO m => ReaderT FilePath m a

-- | Log writing a file.
logWriting :: FilePath -> FileLocation ()
logWriting path = liftIO $ putStrLn $ "> writing: " <> path

-- | Project root relative path to compiled script storage directory.
scriptOutDirOnchain :: FilePath
scriptOutDirOnchain = ".." </> "offchain" </> "compiled"

scriptOutDirOffchain :: FilePath
scriptOutDirOffchain = "compiled"

-- | Util to name a plutus file.
plutusFile :: String -> FileLocation FilePath
plutusFile fileName = do
    location <- ask
    return $ location </> (fileName <> ".plutus")

-- | Used for testing and scaffolding.
constTrueValidatorFile :: FileLocation FilePath
constTrueValidatorFile = plutusFile "ConstTrueValidator"

-- | Thrift Finance Compiled file.
tfTokenFile :: FileLocation FilePath
tfTokenFile =  plutusFile "TfTokenMp"

-- | Thrift Finance Compiled script.
tfValidatorFile :: FileLocation FilePath
tfValidatorFile = plutusFile "TfValidator"

trivialTokenFile :: FileLocation FilePath
trivialTokenFile = plutusFile "TrivialToken"

trivialRewardTokenFile :: FileLocation FilePath
trivialRewardTokenFile = plutusFile "TrivialRewardToken"

otherConstTrueValidatorFile :: FileLocation FilePath
otherConstTrueValidatorFile = plutusFile "OtherConstTrueValidator"

-- | Deletes existing directory if it already exists and creates a new one.
refreshDirectory :: FileLocation ()
refreshDirectory = do
  dirPath  <- ask
  liftIO $ putStrLn ("> Refreshing: " <> dirPath <> " directory.")
  liftIO $ removeDirectoryRecursive dirPath `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)
  liftIO $ createDirectory dirPath

-- | ThriftFinance's TokenNames
tfDepositTokenName :: TokenName
tfDepositTokenName = TokenName "ThriftFinanceDeposit"

tfActiveTokenName :: TokenName
tfActiveTokenName = TokenName "ThriftFinanceActive"

trivialTokenName :: TokenName
trivialTokenName = TokenName "TrivialToken"

trivialRewardTokenName :: TokenName
trivialRewardTokenName = TokenName "TrivialRewardToken"
