{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Spec.LockStarterNFTContract (tests) where
import PlutusTx.Prelude
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api        as V2
import Plutus.V1.Ledger.Value      as Value
import Test.Tasty
import Test.Tasty.QuickCheck
import LockStarterNFTContract  as NFTLock

scriptParams :: ScriptParameters
scriptParams = ScriptParameters 
  { starterPid = ""
  , starterTkn = ""
  , mainPkh    = ""
  , multiPkhs  = []
  }

tests :: [TestTree]
tests = [ testProperty "Test"  True
        ]
        