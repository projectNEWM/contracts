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
import Plutus.Trace.Emulator (EmulatorTrace, ContractInstanceTag)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Contract (Contract, ContractError)
import Plutus.Script.Utils.Ada qualified as Ada
import Control.Monad (void, forever)
import Plutus.Contract.Test
import Ledger.Address
import Ledger.CardanoWallet qualified as CW
import Wallet.Emulator.Wallet (signPrivateKeys)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as Tx
import Ledger.Value.CardanoAPI qualified as CValue

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

nftLockContract :: Contract () NFTLock.NFTLockSchema ContractError ()
nftLockContract = NFTLock.contract

starterPolicyId :: V2.CurrencySymbol
starterPolicyId = V2.CurrencySymbol { V2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [38, 144, 61, 231, 221, 148, 253, 203, 89, 253, 43, 89, 128, 168, 202, 79, 247, 31, 6, 47, 126, 210, 88, 89, 203, 38, 232, 127] }

starterTokenName :: V2.TokenName
starterTokenName = V2.TokenName { V2.unTokenName = UsefulFuncs.createBuiltinByteString [78, 69, 87, 77, 95] }

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

tokenizedPolicyId :: V2.CurrencySymbol
tokenizedPolicyId = V2.CurrencySymbol { V2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [71, 59, 65, 205, 192, 50, 86, 98, 198, 115, 24, 173, 100, 131, 186, 139, 50, 181, 160, 226, 154, 136, 16, 10, 235, 195, 179, 114] }

nftLockDatum :: CustomDatumType
nftLockDatum = CustomDatumType
  { cdtNewmPid = tokenizedPolicyId
  , cdtNumber  = 0
  , cdtPrefix  = "4e45574d5f0a"
  }

tokenizedTokenName :: V2.TokenName
tokenizedTokenName = V2.TokenName { V2.unTokenName = (nftName (cdtPrefix nftLockDatum) (cdtNumber nftLockDatum))}

tokenizedValue :: V2.Value
tokenizedValue = V2.singleton (cdtNewmPid nftLockDatum) tokenizedTokenName 1

sendStarterTokenTrace :: EmulatorTrace ()
sendStarterTokenTrace = do
  hdl <- Trace.activateContractWallet w1 nftLockContract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock" hdl (nftLockParams, nftLockDatum, starterToken)
  void $ Trace.waitNSlots 1

mintTokenTrace :: EmulatorTrace ()
mintTokenTrace = do
  hdl <- Trace.activateContractWallet w1 nftLockContract
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"mint" hdl (nftLockParams, nftLockDatum, starterToken)
  void $ Trace.waitNSlots 1

tests :: [TestTree]
tests = 
  [
    checkPredicate "Expose 'lock' endpoint, 'mint' endpoint, 'burn' endpoint"
    (endpointAvailable @"lock" nftLockContract t1 .&&.
     endpointAvailable @"mint" nftLockContract t1 .&&.
     endpointAvailable @"burn" nftLockContract t1)
    $ void $ Trace.activateContractWallet w1 nftLockContract
  
  , checkPredicate "The 'lock' endpoint submits a transaction"
    (assertNoFailedTransactions .&&.
     anyUnbalancedTx nftLockContract t1)
    sendStarterTokenTrace
  
  , checkPredicate "The 'mint' endpoint is available after locking token"
    (endpointAvailable @"mint" nftLockContract t2)
    $ do
      void $ Trace.activateContractWallet w2 nftLockContract
      sendStarterTokenTrace
  
  , checkPredicate "Attempt to mint a token after locking the starter token"
    (assertNoFailedTransactions)
    $ do
      sendStarterTokenTrace
      mintTokenTrace
  ]
        