import Prelude
import Test.Tasty (defaultMain, testGroup)
import Spec.LockStarterNFTContract  as NFTLocking
--------------------------------------------------------------------------------
main :: IO ()
main =
  defaultMain $
    testGroup
      "contracts"
      [ testGroup "NFT Lock" NFTLocking.tests
      ]