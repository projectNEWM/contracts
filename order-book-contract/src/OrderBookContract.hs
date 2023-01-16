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
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = FullSwap UTxOData  |
                          PartSwap UTxOData  |
                          Update |
                          Remove
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
  -}
  case datum of
    (Swap ptd have want sd) -> let !walletPkh  = ptPkh ptd
                                   !walletAddr = createAddress walletPkh (ptSc ptd)
                                   !txSigners  = V2.txInfoSignatories info
      in case redeemer of
        -- | Remove the utxo back to the wallet
        Remove -> traceIfFalse "sign" (signedBy txSigners walletPkh)                   -- wallet must sign it
               && traceIfFalse "pay"  (findExactPayout txOutputs walletAddr thisValue) -- token must go back to wallet
               && traceIfFalse "ins"  (nInputs txInputs 1)                             -- a single input datum

        -- | Update the order book utxo with new sale information.
        Update -> 
          case getOutboundDatum contTxOutputs of
            (Swap ptd' _ _ _) -> traceIfFalse "sign"  (signedBy txSigners walletPkh) -- wallet must sign it
                              && traceIfFalse "ins"   (isNInputs txInputs 1)         -- single script input
                              && traceIfFalse "outs"  (isNOutputs contTxOutputs 1)   -- single script output
                              && traceIfFalse "owner" (ptd == ptd')                  -- owner must remain

        -- | Fully Swap two utxos.
        (FullSwap utxo) -> let !txId = createTxOutRef (uTx utxo) (uIdx utxo)
          in case getDatumByTxId txId of
            (Swap ptd' have' want' sd') -> let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                               !thisToken = TokenSwapInfo have sd
                                               !thatToken = TokenSwapInfo want' sd'
                                        in traceIfFalse "ins"   (isNInputs txInputs 2)                          -- double script inputs
                                        && traceIfFalse "selfy" (ptd /= ptd')                                   -- Must be different datums
                                        && traceIfFalse "pay"   (findExactPayout txOutputs otherAddr thisValue) -- token must go back to wallet
                                        && traceIfFalse "pair"  (checkMirrorTokens have want')                  -- mirrored have and want tokens.
                                        && traceIfFalse "slip"  (checkIfInSlippageRange thisToken thatToken)    -- slippage is in range

        -- | Partially Swap Two UTxOs.
        -- TODO
        (PartSwap utxo) -> let !txId = createTxOutRef (uTx utxo) (uIdx utxo)
          in case getDatumByTxId txId of
            (Swap ptd' have' want' sd') -> let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                               !thisDatum = Swap ptd have want sd
                                               !thatDatum = Swap ptd' have' want' sd'
                                        in traceIfFalse "ins"   (isNInputs txInputs 2)            -- double script inputs
                                        && traceIfFalse "outs"  (isNOutputs contTxOutputs 1)      -- single script output
                                        && traceIfFalse "pair"  (checkMirrorTokens have want')    -- mirrored have and want tokens.
                                        && traceIfFalse "selfy" (ptd /= ptd')                     -- Must be different datums
                                        && traceIfFalse "slip"  (checkEffectiveSlippage thisDatum thatDatum)         -- slippage is in range
                                        -- calculate what gets traded here
                                        -- who goes back is what doesnt get consumed
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo context

    txInputs :: [V2.TxInInfo]
    txInputs = V2.txInfoInputs info

    txOutputs :: [V2.TxOut]
    txOutputs = V2.txInfoOutputs info
    
    contTxOutputs :: [V2.TxOut]
    contTxOutputs = V2.getContinuingOutputs context

    thisValue :: V2.Value
    thisValue =
      case V2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate"
        Just input -> V2.txOutValue $ V2.txInInfoResolved input

    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }

    getOutboundDatum :: [V2.TxOut] -> OrderBookDatum
    getOutboundDatum []     = traceError "Nothing Found"
    getOutboundDatum (x:xs) =
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> getOutboundDatum xs
        (V2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline
    
    getDatumByTxId :: V2.TxOutRef -> OrderBookDatum
    getDatumByTxId txId = 
      case V2.txOutDatum $ V2.txInInfoResolved $ txInFromTxRef txInputs txId of
        V2.NoOutputDatum       -> traceError "No Datum"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline

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
