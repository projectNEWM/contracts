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
  , orderBookContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
-- import qualified PlutusTx.AssocMap              as AM
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Contexts      as V2
import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
-- new stuff
import           OrderBookDatum
import           OrderBookRedeemer
import           ReducedFunctions
-- import qualified Plutonomy
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = FullSwap UTxOData |
                          PartSwap UTxOData |
                          Update            |
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
        Remove -> (signedBy txSigners walletPkh)                   -- wallet must sign it
               && (findPayout txOutputs walletAddr thisValue) -- token must go back to wallet
               && (nInputs txInputs scriptAddr 1)                             -- a single input datum

        -- | Update the order book utxo with new sale information.
        Update -> 
          case getOutboundDatum contTxOutputs of
            (Swap ptd' _ _ _) -> (signedBy txSigners walletPkh) -- wallet must sign it
                              && (nInputs txInputs scriptAddr 1)         -- single script input
                              && (nOutputs contTxOutputs 1)   -- single script output
                              && (ptd == ptd')                  -- owner must remain constant

        -- | Fully Swap two utxos.
        (FullSwap utxo) -> let !txId = createTxOutRef (uTx utxo) (uIdx utxo)
          in case getDatumByTxId txId of
            (Swap ptd' _ want' sd') -> let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                           !thisToken = TokenSwapInfo have sd
                                           !thatToken = TokenSwapInfo want' sd'
              in traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)                         -- double datum inputs
              -- && traceIfFalse "red"  (nRedeemers redeemers 2)                     -- double script spends
              && traceIfFalse "pay"  (findPayout txOutputs otherAddr thisValue)   -- token must go back to other wallet
              && traceIfFalse "pair" (checkMirrorTokens have want')               -- mirrored have and want tokens.
              && traceIfFalse "slip" (checkIfInSlippageRange thisToken thatToken) -- slippage is in range
              && traceIfFalse "lie"  (checkValueHolds have thisValue)             -- must have what you claim to have

        -- | Partially Swap Two UTxOs.
        (PartSwap utxo) -> let !txId = createTxOutRef (uTx utxo) (uIdx utxo)
          in case getDatumByTxId txId of
            (Swap ptd' have' want' sd') -> let !otherAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                               !thisDatum = Swap ptd have want sd
                                               !thatDatum = Swap ptd' have' want' sd'
                                               !thisToken = TokenSwapInfo have sd
                                               !thatToken = TokenSwapInfo want' sd'
                                               !thatValue = createValue want'
                in traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)                                         -- double datum inputs
                -- && traceIfFalse "red"  (nRedeemers redeemers 2)                                     -- double script spends
                && traceIfFalse "outs" (nOutputs contTxOutputs 1)                                   -- single script output
                && traceIfFalse "pair" (checkMirrorTokens have want')                               -- mirrored have and want tokens.
                && traceIfFalse "slip" (checkEffectiveSlippage thisDatum thatDatum)                 -- slippage is in range
                && traceIfFalse "full" (not $ checkIfInSlippageRange thisToken thatToken)           -- not full swap
                && traceIfFalse "lie"  (checkValueHolds have thisValue)                             -- must have what you claim to have
                && traceIfFalse "pay"  (checkPartialPayout thisDatum thatDatum otherAddr thatValue) -- this value goes where
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo context

    txInputs :: [V2.TxInInfo]
    txInputs = V2.txInfoInputs info

    txOutputs :: [V2.TxOut]
    txOutputs = V2.txInfoOutputs info
    
    validatingInput :: V2.TxOut
    validatingInput = ownInput context

-- | create a list of staking credential and reward in lovelace
    -- redeemers :: [(V2.ScriptPurpose, V2.Redeemer)]
    -- redeemers = AM.toList $ V2.txInfoRedeemers info

    thisValue :: V2.Value
    thisValue = V2.txOutValue validatingInput
    
    scriptAddr :: V2.Address
    scriptAddr = V2.txOutAddress validatingInput

    contTxOutputs :: [V2.TxOut]
    contTxOutputs = getScriptOutputs txOutputs scriptAddr

    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }

    getOutboundDatum :: [V2.TxOut] -> OrderBookDatum
    getOutboundDatum []     = traceError "Nothing Found On Cont"
    getOutboundDatum (x:xs) =
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> getOutboundDatum xs
        (V2.OutputDatumHash _) -> traceError "Embedded Datum On Cont"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data On Cont"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline
    
    checkOutboundDatum :: [V2.TxOut] -> V2.Value -> OrderBookDatum -> Bool
    checkOutboundDatum []     _   _ = traceError "Nothing Found Cont Validation"
    checkOutboundDatum (x:xs) val (Swap ptd have want sd) =
      if V2.txOutValue x == val -- strict value continue
        then
          case V2.txOutDatum x of
            V2.NoOutputDatum       -> traceError "No Datum Cont Validation"
            (V2.OutputDatumHash _) -> traceError "Embedded Datum Cont Validation"
            -- inline datum only
            (V2.OutputDatum (V2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data Cont Validation"
                Just inline -> 
                  case PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline of
                    (Swap ptd' have' want' sd') -> (ptd == ptd')
                                                && (sd == sd')
                                                && (have == have')
                                                && (want == want')
        else checkOutboundDatum xs val (Swap ptd have want sd)

    -- this needs to not be able to reference the txId that is being spent
    getDatumByTxId :: V2.TxOutRef -> OrderBookDatum
    getDatumByTxId txId = 
      case V2.txOutDatum $ V2.txInInfoResolved $ txInFromTxRef txInputs txId of
        V2.NoOutputDatum       -> traceError "No Datum On TxId"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum On TxId"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data On TxId"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OrderBookDatum inline

    checkPartialPayout :: OrderBookDatum -> OrderBookDatum -> V2.Address -> V2.Value -> Bool
    checkPartialPayout (Swap pdt have want sd) (Swap _ have' want' _) otherAddr thatValue = 
      let !thisPrice = HaveWantInfo (tiAmt have) (tiAmt want)
          !thatPrice = HaveWantInfo (tiAmt have') (tiAmt want')
      in if checkContValue thisPrice thatPrice == True
        then (findPayout txOutputs otherAddr thisValue) -- payout the validating value to the other address
        else                                            -- else split the payout to the wscript and the other address
          let !partialValue = thisValue - thatValue
              !newHave = subtractTokenInfo have want'
              !newWant = subtractTokenInfo want have'
          in (findPayout contTxOutputs scriptAddr partialValue)
          && (findPayout txOutputs otherAddr thatValue)
          && (checkOutboundDatum contTxOutputs partialValue (Swap pdt newHave newWant sd))
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
----------- plutonomy makes it cost more for some reason
-- wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrappedValidator x y z = check (mkValidator (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y) (V2.unsafeFromBuiltinData z))

-- validator :: Validator
-- -- validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
-- validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

-- orderBookContractScriptShortBs :: SBS.ShortByteString
-- orderBookContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

-- orderBookContractScript :: PlutusScript PlutusScriptV2
-- orderBookContractScript = PlutusScriptSerialised orderBookContractScriptShortBs