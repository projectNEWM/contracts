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
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module OrderBookContract
  ( orderBookContractScript
  , orderBookContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
import           DataTypes
import           HelperFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Swap   SwapData    |
                          Update IncreaseADA |
                          Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Swap,   0 )
                                                , ( 'Update, 1 )
                                                , ( 'Remove, 2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: OrderBookData -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  {- | Order Book Contract

    This contract allows an order book style dex to be built on Cardano with
    minimal batching required. It will allow mirrored swaps as well as partial
    order filling.
  -}
  case redeemer of
    -- | Swap two utxos according to the order book method.
    (Swap sData) ->
      let txId = createTxOutRef (sTx sData) (sIdx sData)
      in case getDatumByTxId txId of
        Nothing -> traceIfFalse "getDatumByTxId Error" False
        Just otherDatum -> do
          { let walletAAddr = createAddress (obPkh datum)      (obSc datum)
          ; let walletBAddr = createAddress (obPkh otherDatum) (obSc otherDatum)
          ; let isPart      = checkIfPartialSwap datum otherDatum
          ; let a = traceIfFalse "Incorrect In/Outs"  $ isNInputs txInputs 2 && isNOutputs contTxOutputs isPart -- single script input
          ; let b = traceIfFalse "No Mirrorred Pairs" $ checkMirrorredDatums datum otherDatum                   -- mirrored have and want tokens.
          ; let c = traceIfFalse "Hold Enough Token"  $ checkIfHoldingEnough                                    -- must have what is claimed
          ; let d = traceIfFalse "Incorrect Slippage" $ checkIfInEffectiveSlippageRange datum otherDatum        -- slippage is in range
          ;         traceIfFalse "Swap Error"         $ all (==(True :: Bool)) [a,b,c,d]
          }
    -- | Update the order book utxo with new sale information.
    (Update iData) ->
      let extraAda = (pow (-1) (iExtraAdaFlag iData)) * (iExtraAda iData)
          feeDiff  = (pow (-1) (iFeeDiffFlag  iData)) * (iFeeDiff  iData)
          incDiff  = (pow (-1) (iIncDiffFlag  iData)) * (iIncDiff  iData)
          additionalValue = validatingValue + adaValue extraAda + adaValue feeDiff + adaValue incDiff
      in case getContinuingDatum contTxOutputs additionalValue of
        Nothing        -> traceIfFalse "getContinuingDatum Error" False
        Just contDatum -> do
          { let a = traceIfFalse "Incorrect Signer"    $ ContextsV2.txSignedBy info (obPkh datum)           -- wallet must sign it
          ; let b = traceIfFalse "Single Script UTxO"  $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single script input
          ; let c = traceIfFalse "Incorrect New State" $ updateOrderBookData datum contDatum                -- update allowed information
          ; let d = traceIfFalse "Incorrect Ada Amt"   $ verifyExtraADA datum contDatum iData               -- verify the fee and incentive add up
          ;         traceIfFalse "Update Error"        $ all (==(True :: Bool)) [a,b,c,d]
          }
    -- | Remove the order book utxo back to the wallet
    Remove -> do
      { let walletPkh  = obPkh datum
      ; let walletSc   = obSc datum
      ; let walletAddr = createAddress walletPkh walletSc
      ; let a = traceIfFalse "Incorrect Signer"   $ ContextsV2.txSignedBy info walletPkh                          -- wallet must sign it
      ; let b = traceIfFalse "Value Not Returned" $ isAddrGettingPaidExactly txOutputs walletAddr validatingValue -- token must go back to wallet
      ; let c = traceIfFalse "Single Script UTxO" $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0            -- single script input
      ;         traceIfFalse "Remove Error"       $ all (==(True :: Bool)) [a,b,c]
      }
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate"
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    checkIfPartialSwap :: OrderBookData -> OrderBookData -> Integer
    checkIfPartialSwap thisDatum thatDatum =
      if checkIfInSlippageRange thisDatum thatDatum
        then 0 -- full swap
        else 1 -- partial swap

    checkIfHoldingEnough :: Bool
    checkIfHoldingEnough = Value.valueOf validatingValue (obHavePid datum) (obHaveTkn datum) == (obHaveAmt datum) 

    getContinuingDatum :: [PlutusV2.TxOut] -> PlutusV2.Value -> Maybe OrderBookData
    getContinuingDatum []     _   = Nothing
    getContinuingDatum (x:xs) val =
      if PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> getContinuingDatum xs val -- skip datumless
            (PlutusV2.OutputDatumHash _) -> getContinuingDatum xs val -- skip embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getContinuingDatum xs val
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @OrderBookData inline
        else getContinuingDatum xs val
    
    getDatumByTxId :: PlutusV2.TxOutRef -> Maybe OrderBookData
    getDatumByTxId txId = 
      case ContextsV2.findTxInByTxOutRef txId info of
        Nothing -> Nothing
        Just txIn -> 
          case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved txIn of
            PlutusV2.NoOutputDatum       -> Nothing -- skip datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- skip embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @OrderBookData inline
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

orderBookContractScriptShortBs :: SBS.ShortByteString
orderBookContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

orderBookContractScript :: PlutusScript PlutusScriptV2
orderBookContractScript = PlutusScriptSerialised orderBookContractScriptShortBs
