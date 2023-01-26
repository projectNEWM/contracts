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
module FractionalSaleContract
  ( fractionalSaleContractScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley                             ( PlutusScript (..)
                                                                 , PlutusScriptV2 )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V1.Ledger.Value                          as Value
import qualified Plutus.V1.Ledger.Scripts                        as Scripts
import qualified Plutus.V2.Ledger.Contexts                       as V2
import qualified Plutus.V2.Ledger.Api                            as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           FractionalSaleDatum
import           FractionalSaleRedeemer
import           ReducedFunctions
import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Purchase SaleData
  | Update
  | Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Purchase, 0 )
                                                , ( 'Update,   1 )
                                                , ( 'Remove,   2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: FractionalSaleDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  {- | Fractional Sale Contract

    This contract handles the initial sale of fractional tokens.
  -}
  case (datum, redeemer) of
    
    -- | Remove Sale From Contract
    (Sale ptd _ _, Remove) ->
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
    
    -- | Update the sale
    (Sale ptd _ _, Update) ->
      let !walletPkh  = ptPkh ptd
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
      in case getOutboundDatum contOuts of
        (Sale ptd' _ _) -> 
          (signedBy txSigners walletPkh) &&  -- wallet must sign it
          (nInputs txIns scriptAddr 1)   &&  -- single script input
          (nOutputs contOuts 1)          &&  -- single script output
          (ptd == ptd')                      -- owner remains constant
    
    -- | Purchase from the sale
    (Sale ptd bSize pay, Purchase sd) ->
      let !info       = V2.scriptContextTxInfo context
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
          !walletAddr = createAddress (ptPkh ptd) (ptSc ptd)
          !bAmt       = bundleSize sd
          !outValue   = multiplyValue pay bAmt
          !retValue   = thisValue - multiplyValue bSize bAmt
          !thisDatum  = Sale ptd bSize pay
      in
        traceIfFalse "ins" (nInputs txIns scriptAddr 1)                     &&  -- 2 script inputs
        traceIfFalse "out" (nOutputs contOuts 1)                            &&  -- 1 script output
        traceIfFalse "val" (validBundle sd)                                 &&  -- valid bundle size
        traceIfFalse "pay" (findPayout txOuts walletAddr outValue)          &&  -- token is paid
        traceIfFalse "dat" (checkOutboundDatum contOuts retValue thisDatum)     -- datum is correct
  where
    checkOutboundDatum :: [V2.TxOut] -> V2.Value -> FractionalSaleDatum -> Bool
    checkOutboundDatum outs val dat = checkOutboundDatum' outs val dat
      where
        checkOutboundDatum' :: [V2.TxOut] -> V2.Value -> FractionalSaleDatum -> Bool
        checkOutboundDatum' []     _   _ = traceError "Nothing Found"
        checkOutboundDatum' (x:xs) val (Sale ptd bSize pay)
          | Value.geq (V2.txOutValue x) val =
            case V2.txOutDatum x of
              V2.NoOutputDatum              -> traceError "No Datum Validation"
              (V2.OutputDatumHash _)        -> traceError "Embed Datum Validation"
              (V2.OutputDatum (V2.Datum d)) -> -- inline datum only
                case PlutusTx.fromBuiltinData d of
                  Nothing     -> traceError "Bad Data Cont Validation"
                  Just inline -> 
                    case PlutusTx.unsafeFromBuiltinData @FractionalSaleDatum inline of
                      (Sale ptd' bSize' pay') -> 
                        (ptd   == ptd')   &&
                        (bSize == bSize') &&
                        (pay   == pay') 
          | otherwise = checkOutboundDatum' xs val (Sale ptd bSize pay)

    -- gets the first inline datum from a list of txouts
    getOutboundDatum :: [V2.TxOut] -> FractionalSaleDatum
    getOutboundDatum outs = getOutboundDatum' outs
      where
        getOutboundDatum' :: [V2.TxOut] -> FractionalSaleDatum
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
                  PlutusTx.unsafeFromBuiltinData @FractionalSaleDatum inline
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

fractionalSaleContractScriptShortBs :: SBS.ShortByteString
fractionalSaleContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

fractionalSaleContractScript :: PlutusScript PlutusScriptV2
fractionalSaleContractScript = PlutusScriptSerialised fractionalSaleContractScriptShortBs