module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.ThriftFinance ()
import Prelude (IO)

-- | @since 0.1
main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "ThriftFinance"
    []
