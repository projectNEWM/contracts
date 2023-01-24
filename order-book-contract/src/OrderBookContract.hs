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
module OrderBookContract
  ( orderBookContractScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..)
                                                , PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Contexts      as V2
import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
-- new stuff
import           OrderBookDatum
import           OrderBookRedeemer
import           ReducedFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = FullSwap UTxOData
  | PartSwap UTxOData
  | Update
  | Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'FullSwap, 0 )
                                                , ( 'PartSwap, 1 )
                                                , ( 'Update,   2 )
                                                , ( 'Remove,   3 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: OrderBookDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  {- | Order Book Contract

    This contract allows an order book style dex to be built on Cardano with
    minimal batching required. It will allow mirrored swaps as well as partial
    order filling.

    The variable thisValue is the currently validating value.
  -}
  case (datum, redeemer) of
    
    -- | Remove the UTxO back to the wallet
    (Swap ptd _ _ _, Remove) ->
      let !walletPkh  = ptPkh ptd
          !walletAddr = createAddress walletPkh (ptSc ptd)
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
      in
        (signedBy txSigners walletPkh)           &&  -- wallet must sign
        (findPayout txOuts walletAddr thisValue) &&  -- must pay to wallet
        (nInputs txIns scriptAddr 1)                 -- single input datum

    -- | Update the UTxO with new swap information.
    (Swap ptd _ _ _, Update) ->
      let !walletPkh  = ptPkh ptd
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
      in case getOutboundDatum contOuts of
        (Swap ptd' _ _ _) -> 
          (signedBy txSigners walletPkh) &&  -- wallet must sign it
          (nInputs txIns scriptAddr 1)   &&  -- single script input
          (nOutputs contOuts 1)          &&  -- single script output
          (ptd == ptd')                      -- owner remains constant

    -- | Fully swap two UTxOs.
    (Swap ptd have _ sd, FullSwap utxo) ->
      let !txId       = createTxOutRef (uTx utxo) (uIdx utxo)
          !info       = V2.scriptContextTxInfo context
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
      in case getDatumByTxId txIns txId of
        (Swap ptd' _ want' sd') ->
          let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
              !thisToken = TokenSwapInfo have sd
              !thatToken = TokenSwapInfo want' sd'
              !outValue = createValue have
          in
            traceIfFalse "ins" (nInputs txIns scriptAddr 2)           &&  -- 2 script inputs
            traceIfFalse "own" (ptd /= ptd')                          &&  -- cant ref self
            traceIfFalse "pay" (findPayout txOuts otherAddr outValue) &&  -- token is paid
            traceIfFalse "mir" (checkMirrorTokens have want')         &&  -- mirror tkn only
            traceIfFalse "sli" (inSlipRange thisToken thatToken)      &&  -- slip in range
            traceIfFalse "lie" (checkValueHolds have thisValue)           -- must have token
    

    -- | Partially swap two UTxOs.
    (Swap ptd have want sd, PartSwap utxo) ->
      let !txId = createTxOutRef (uTx utxo) (uIdx utxo)
          !info       = V2.scriptContextTxInfo context
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
      in case getDatumByTxId txIns txId of
        (Swap ptd' have' want' sd') -> 
          let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
              !thisDatum = Swap ptd have want sd
              !thatDatum = Swap ptd' have' want' sd'
              !thisToken = TokenSwapInfo have sd
              !thatToken = TokenSwapInfo want' sd'
              !thatValue = createValue want'
          in
            traceIfFalse "ins" (nInputs txIns scriptAddr 2)            &&  -- 2 script inputs
            traceIfFalse "out" (nOutputs contOuts 1)                   &&  -- 1 script output
            traceIfFalse "own" (ptd /= ptd')                           &&  -- cant ref self
            traceIfFalse "mir" (checkMirrorTokens have want')          &&  -- mirrored tokens
            traceIfFalse "eff" (inEffectiveRange thisDatum thatDatum)  &&  -- slipp in range
            traceIfFalse "sli" (not $ inSlipRange thisToken thatToken) &&  -- not full swap
            traceIfFalse "lie" (checkValueHolds have thisValue)        &&  -- must have tkn
            traceIfFalse "par" (isPartialPay 
                                  thisDatum thatDatum 
                                  otherAddr scriptAddr
                                  thatValue
                                  txOuts contOuts
                               ) -- part payback
  
  where
    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }

    getOutboundDatum :: [V2.TxOut] -> OrderBookDatum
    getOutboundDatum outs = getOutboundDatum' outs
      where
        getOutboundDatum' :: [V2.TxOut] -> OrderBookDatum
        getOutboundDatum' []     = traceError "Nothing Found On Cont"
        getOutboundDatum' (x:xs) =
          case V2.txOutDatum x of
            V2.NoOutputDatum       -> getOutboundDatum' xs
            (V2.OutputDatumHash _) -> traceError "Embedded Datum On Cont"
            -- inline datum only
            (V2.OutputDatum (V2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data On Cont"
                Just inline ->
                  PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline
    
    checkOutboundDatum :: [V2.TxOut] -> V2.Value -> OrderBookDatum -> Bool
    checkOutboundDatum outs val dat = checkOutboundDatum' outs val dat
      where
        checkOutboundDatum' :: [V2.TxOut] -> V2.Value -> OrderBookDatum -> Bool
        checkOutboundDatum' []     _   _ = traceError "Nothing Found On Check"
        checkOutboundDatum' (x:xs) val (Swap ptd have want sd)
          | Value.geq (V2.txOutValue x) val =
            case V2.txOutDatum x of
              V2.NoOutputDatum              -> traceError "No Datum Validation"
              (V2.OutputDatumHash _)        -> traceError "Embed Datum Validation"
              (V2.OutputDatum (V2.Datum d)) -> -- inline datum only
                case PlutusTx.fromBuiltinData d of
                  Nothing     -> traceError "Bad Data Cont Validation"
                  Just inline -> 
                    case PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline of
                      (Swap ptd' have' want' sd') -> 
                        (ptd  == ptd')  &&
                        (sd   == sd')   &&
                        (have == have') &&
                        (want == want') 
          | otherwise = checkOutboundDatum' xs val (Swap ptd have want sd)

    getDatumByTxId :: [V2.TxInInfo] -> V2.TxOutRef -> OrderBookDatum
    getDatumByTxId txIns txId = 
      case V2.txOutDatum $ V2.txInInfoResolved $ txInFromTxRef txIns txId of
        V2.NoOutputDatum       -> traceError "No Datum On TxId"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum On TxId"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data On TxId"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline

    isPartialPay 
      :: OrderBookDatum 
      -> OrderBookDatum 
      -> V2.Address 
      -> V2.Address 
      -> V2.Value
      -> [V2.TxOut] -- all outputs
      -> [V2.TxOut] -- continue outputs
      -> Bool
    isPartialPay
      (Swap pdt have want sd) (Swap _ have' want' _)
      otherAddr scriptAddr
      thatValue 
      txOuts contOuts =
      let !thisPrice = HaveWantInfo (tiAmt have) (tiAmt want)
          !thatPrice = HaveWantInfo (tiAmt have') (tiAmt want')
          !outValue  = createValue have
      in
        if checkContValue thisPrice thatPrice == True
        then (findPayout txOuts otherAddr outValue) -- pay the validating value
        else                                        -- split the return
          let !partialValue = outValue - thatValue
              !newHave = subtractTokenInfo have want'
              !newWant = subtractTokenInfo want have'
          in
            (findPayout contOuts scriptAddr partialValue)                           &&
            (findPayout txOuts otherAddr thatValue)                                 &&
            (checkOutboundDatum contOuts partialValue (Swap pdt newHave newWant sd)) 
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: V2.Validator
validator' = V2.mkValidatorScript
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