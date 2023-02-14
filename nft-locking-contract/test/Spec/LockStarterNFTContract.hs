{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Spec.LockStarterNFTContract (tests) where
import PlutusTx.Prelude
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api        as V2
import Plutus.V1.Ledger.Value      as Value
import qualified UsefulFuncs            ( createBuiltinByteString )
import Test.Tasty
-- import Test.Tasty.QuickCheck
import LockStarterNFTContract  as NFTLock
-- things needed for testing
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Contract (Contract, ContractError)
import Plutus.Script.Utils.Ada qualified as Ada
import Control.Monad (void)
import Plutus.Contract.Test
import Ledger.Address
import Ledger.CardanoWallet qualified as CW
import Wallet.Emulator.Wallet (signPrivateKeys)

nftLockContract :: Contract () NFTLock.NFTLockSchema ContractError ()
nftLockContract = NFTLock.contract

starterPolicyId :: V2.CurrencySymbol
starterPolicyId = V2.CurrencySymbol { V2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [172,171] }

starterTokenName :: V2.TokenName
starterTokenName = V2.TokenName { V2.unTokenName = UsefulFuncs.createBuiltinByteString [190,239] }

starterToken :: V2.Value
starterToken = Ada.lovelaceValueOf 10 <> V2.singleton starterPolicyId starterTokenName 1

nftLockParams :: NFTLock.ScriptParameters
nftLockParams = NFTLock.ScriptParameters 
  { starterPid = starterPolicyId
  , starterTkn = starterTokenName
  , mainPkh    = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w1
  , multiPkhs  = [ unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w2
                 , unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w3
                 , unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w4
                 ]
  }

nftLockDatum :: CustomDatumType
nftLockDatum = CustomDatumType
  { cdtNewmPid = ""
  , cdtNumber  = 0
  , cdtPrefix  = "4e45574d5f0a"
  }

sendStarterTokenTrace :: EmulatorTrace ()
sendStarterTokenTrace = do
  hdl <- Trace.activateContractWallet w1 nftLockContract
  Trace.callEndpoint @"lock" hdl (nftLockParams, nftLockDatum, starterToken)
  void $ Trace.waitNSlots 1


mintTokenTrace :: EmulatorTrace ()
mintTokenTrace = do
  hdl <- Trace.activateContractWallet w1 nftLockContract
  Trace.callEndpoint @"lock" hdl (nftLockParams, nftLockDatum, starterToken)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"mint" hdl (nftLockParams, nftLockDatum, starterToken)
  void $ Trace.waitNSlots 1

tests :: [TestTree]
tests = 
  [ checkPredicate "Send Lovelace with a simple datum" 
      (assertNoFailedTransactions)
      sendStarterTokenTrace
  
  , checkPredicate "Mint a token from the datum" 
      (assertNoFailedTransactions) 
      mintTokenTrace
  ]
        