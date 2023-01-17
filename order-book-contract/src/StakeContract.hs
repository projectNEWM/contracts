{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module StakeContract
  ( stakingPlutusScript
  , stakingScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified PlutusTx.AssocMap              as AM
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
-- 
import           OrderBookDatum
import           OrderBookRedeemer
import           ReducedFunctions


{-
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | The only allowed pool for staking.
-------------------------------------------------------------------------------
poolId :: V2.PubKeyHash
poolId = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [138, 119, 206, 79, 252, 12, 105, 4, 25, 103, 90, 165, 57, 109, 249, 163, 140, 156, 210, 14, 54, 72, 61, 45, 36, 101, 206, 134] }
-------------------------------------------------------------------------------
-- | The reward payout address.
-------------------------------------------------------------------------------
payoutPkh :: V2.PubKeyHash
payoutPkh = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }

payoutSc :: V2.PubKeyHash
payoutSc = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [] }

payoutAddr :: V2.Address
payoutAddr = createAddress payoutPkh payoutSc
-------------------------------------------------------------------------------
-- | Create the stake data.
-------------------------------------------------------------------------------
data StakeData = StakeData
  { stakeCred :: V2.ValidatorHash
  -- ^ The staking credential of the staking script.
  }
PlutusTx.unstableMakeIsData ''StakeData
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Withdraw StakeData |
                          Delegate StakeData |
                          FullSwap UTxOData UTxOData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Withdraw, 0 )
                                                , ( 'Delegate, 1 )
                                                , ( 'FullSwap, 2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> Context -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> V2.ScriptContext -> Bool
mkPolicy redeemer' context =
  case redeemer of
    -- handle the withdrawl of staking rewards
    (Withdraw sd) -> do
      { let stakingCred = V2.StakingHash  $ V2.ScriptCredential $ stakeCred sd
      ; let a = traceIfFalse "Bad Withdrawal"   $ checkTheWithdrawal rewardWithdrawal stakingCred
      ;         traceIfFalse "Withdrawal Error" $ all (==True) [a]
      }
    
    -- handle the pool delegation
    (Delegate sd) -> do
      { let stakingCred = V2.StakingHash $ V2.ScriptCredential $ stakeCred sd
      ; let a = traceIfFalse "Bad Deleg Cert"  $ checkTheCerts dCerts stakingCred
      ;         traceIfFalse "Delegate Error"  $ all (==True) [a]
      }
    
    (FullSwap utxoA utxoB) -> True --let !txIdA = createTxOutRef (uTx utxoA) (uIdx utxoA)
                          --         !txIdB = createTxOutRef (uTx utxoB) (uIdx utxoB)
                          --         !inputA = V2.txInInfoResolved $ txInFromTxRef txInputs txIdA 
                          --         !inputB = V2.txInInfoResolved $ txInFromTxRef txInputs txIdB
                          --         !(Swap ptd have want sd)     = getDatumFromTxOut inputA
                          --         !(Swap ptd' have' want' sd') = getDatumFromTxOut inputB
                          --         !thisToken = TokenSwapInfo have sd
                          --         !thatToken = TokenSwapInfo want' sd'
                          --         !valueA = getValueFromTxOut inputA
                          --         !valueB = getValueFromTxOut inputB
                          --         !addrA = createAddress (ptPkh ptd) (ptSc ptd)
                          --         !addrB = createAddress (ptPkh ptd') (ptSc ptd')
                          --  in traceIfFalse "ins"   (isNInputs txInputs 2)                       -- double script inputs
                          --  && traceIfFalse "pay A" (findExactPayout txOutputs addrA valueB)     -- token must go back to wallet
                          --  && traceIfFalse "pay B" (findExactPayout txOutputs addrB valueA)     -- token must go back to wallet
                          --  && traceIfFalse "pair"  (checkMirrorTokens have want')               -- mirrored have and want tokens.
                          --  && traceIfFalse "slip"  (checkIfInSlippageRange thisToken thatToken) -- slippage is in range
                          --  && traceIfFalse "lie"   (checkValueHolds have valueA)                -- must have what you claim to have
                          --  && traceIfFalse "lie"   (checkValueHolds have' valueB)               -- must have what you claim to have
  where
    -- | create the redeemer type from the builtin data
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'
    
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo context
    
    txInputs :: [V2.TxInInfo]
    txInputs = V2.txInfoInputs info

    txOutputs :: [V2.TxOut]
    txOutputs = V2.txInfoOutputs info

    dCerts :: [V2.DCert]
    dCerts = V2.txInfoDCert info

    -- | create a list of staking credential and reward in lovelace
    rewardWithdrawal :: [(V2.StakingCredential, Integer)]
    rewardWithdrawal = AM.toList $ V2.txInfoWdrl info

     -- | check for withdraws then check if the payout address gets the reward
    checkTheWithdrawal :: [(V2.StakingCredential, Integer)] -> V2.StakingCredential -> Bool
    checkTheWithdrawal []     _  = False
    checkTheWithdrawal (x:xs) sc =
      if     traceIfFalse "Incorrect Stake Key"      $ stakeCred == sc                                           -- must be from this stake
        then traceIfFalse "Incorrect Reward Payment" $ isAddrGettingPaidExactly txOutputs payoutAddr payoutValue -- send reward to payout address
        else checkTheWithdrawal xs sc
      where
        stakeCred :: V2.StakingCredential
        stakeCred = fst x

        rewardAmt :: Integer
        rewardAmt = snd x

        payoutValue :: V2.Value
        payoutValue = adaValue rewardAmt

    -- | loop all the certs and check if the stake is going to the right pool
    checkTheCerts :: [V2.DCert] -> V2.StakingCredential -> Bool
    checkTheCerts []     _  = False
    checkTheCerts (x:xs) sc =
      if checkCert x
        then True                -- correct credential and pool
        else checkTheCerts xs sc -- loop all the certs
      where
        -- check that the delegation is correct
        checkCert :: V2.DCert -> Bool
        checkCert cert = 
          case cert of
            -- check for a delegation to stake pool
            (V2.DCertDelegDelegate sc' poolId') -> 
              ( traceIfFalse "Incorrect Stake Key" $ sc     == sc'     ) && -- only this cred can be staked
              ( traceIfFalse "Incorrect Pool Id"   $ poolId == poolId' )    -- must delegate to specific pool id
            _ -> False                                                      -- any other cert fails but not registration
    
    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }
    
    getDatumFromTxOut :: V2.TxOut-> OrderBookDatum
    getDatumFromTxOut txOut = 
      case V2.txOutDatum txOut of
        V2.NoOutputDatum       -> traceError "No Datum"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline
    
    getValueFromTxOut :: V2.TxOut-> V2.Value
    getValueFromTxOut txOut = V2.txOutValue txOut
-------------------------------------------------------------------------------
-- | Compile Information
-------------------------------------------------------------------------------
policy :: V2.StakeValidator
policy = V2.mkStakeValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Utils.mkUntypedStakeValidator mkPolicy

plutusScript :: Scripts.Script
plutusScript = V2.unStakeValidatorScript policy

validator :: V2.Validator
validator = V2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

stakingPlutusScript :: PlutusScript PlutusScriptV2
stakingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

stakingScriptShortBs :: SBS.ShortByteString
stakingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor