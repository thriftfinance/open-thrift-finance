module Main (main) where

-- import Spec.Plutip (trivialProtocolTests)
-- import Spec.Tests.Initialise(initialisePoolTests)
import Spec.Tests.MVP (mvpTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "ThriftFinanceTests"
      [ -- trivialProtocolTests,
        -- initialisePoolTests,
        mvpTests
      ]
