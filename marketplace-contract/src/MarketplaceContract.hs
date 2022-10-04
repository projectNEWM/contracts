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
module MarketplaceContract
  ( marketplaceContractScript
  , marketplaceContractScriptShortBs
  , testData
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
import           OracleDataType
-- testing shit
testData :: OracleData
testData = OracleData
  { oPkh        = ""
  , oSc         = ""
  , oInPid      = ""
  , oInTkn      = ""
  , oInAmt      = 42000000
  , oOutPid     = "acab"
  , oOutTkn     = "beef"
  , oOutAmt     = 8000
  , oAge        = 0
  , oCognoTxId  = ""
  , oCognoIndex = 0
  }
-------------------------------------------------------------------------------
-- | this needs to be exposed in cogno
-------------------------------------------------------------------------------
data CognoDatumType = Cogno  OracleData |
                      Tag    OracleData |
                      Rank   OracleData |
                      Oracle OracleData 
PlutusTx.makeIsDataIndexed ''CognoDatumType [ ( 'Cogno,  0 )
                                            , ( 'Tag,    1 )
                                            , ( 'Rank,   2 )
                                            , ( 'Oracle, 3 )
                                            ]
-------------------------------------------------------------------------------
-- | Oracle starter token value information
-------------------------------------------------------------------------------
oraclePid :: PlutusV2.CurrencySymbol
oraclePid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [10, 87, 27, 224, 106, 116, 163, 98, 171, 247, 253, 32, 55, 140, 211, 170, 210, 242, 157, 96, 68, 116, 88, 176, 182, 76, 149, 97] }

oracleTkn :: PlutusV2.TokenName
oracleTkn = PlutusV2.TokenName { PlutusV2.unTokenName = createBuiltinByteString [115, 116, 97, 114, 116, 101, 114, 95, 116, 111, 107, 101, 110, 95, 48] }

oracleValue :: PlutusV2.Value
oracleValue = Value.singleton oraclePid oracleTkn (1 :: Integer)
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtPkh :: PlutusV2.PubKeyHash
    -- ^ The wallet's public key hash.
    , cdtSc  :: PlutusV2.PubKeyHash
    -- ^ The wallet's staking key hash.
    , cdtPid :: PlutusV2.CurrencySymbol
    -- ^ The pid of what is being sold
    , cdtTkn :: PlutusV2.TokenName
    -- ^ The tkn of what is being sold.
    }
PlutusTx.unstableMakeIsData ''CustomDatumType

-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtPkh a == cdtPkh b ) &&
           ( cdtSc  a == cdtSc  b ) &&
           ( cdtPid a == cdtPid b ) &&
           ( cdtTkn a == cdtTkn b )
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data PurchaseData = PurchaseData
    { pAmt :: Integer
    -- ^ The amount of the paying token is incoming into the contract.
    }
PlutusTx.unstableMakeIsData ''PurchaseData

data CustomRedeemerType = Remove                |
                          Purchase PurchaseData |
                          Extract               |
                          Debug
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove,   0 )
                                                , ( 'Purchase, 1 )
                                                , ( 'Extract,  2 )
                                                , ( 'Debug,    3 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of

    -- Remove the entire utxo, good for after sale is done
    Remove -> do
      { let pkh  = cdtPkh datum
      ; let addr = createAddress pkh (cdtSc datum)
      ; let a = traceIfFalse "Single In/Out Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 0        -- 1 script input 0 script output
      ; let b = traceIfFalse "Bad Signer Tx Error" $ ContextsV2.txSignedBy info pkh                          -- the owner must sign it
      ; let c = traceIfFalse "Addr Not Paid Error" $ isAddrGettingPaidExactly txOutputs addr validatingValue -- mint correct stable only
      ;         traceIfFalse "Stable: Mint Error"  $ all (==True) [a,b,c]
      }
    
    -- arbitrary token purchases
    ( Purchase pd ) -> do
      { let tokenAmt  = pAmt pd
      ; let a = traceIfFalse "Single In/Out Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let b = traceIfFalse "Single Ref In Error" $ isNInputs txRefInputs 1                          -- 1 ref input
      ; let c = traceIfFalse "Datum Continu Error" $ datumAtValidator datum                           -- datum stays the same
      ; let d = traceIfFalse "Continu Value Error" $ convTokenAmount tokenAmt                         -- convert amt to token with oracle
      ;         traceIfFalse "Purchase FTkn Error" $ all (==True) [a,b,c,d]
      }
    
    -- wallet can extract payments and cont sale
    Extract -> do
      { let pkh     = cdtPkh datum
      ; let currAmt = Value.valueOf validatingValue  (cdtPid datum) (cdtTkn datum)
      ; let nextAmt = Value.valueOf valueAtValidator (cdtPid datum) (cdtTkn datum)
      ; let a = traceIfFalse "Single In/Out Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let b = traceIfFalse "Bad Signer Tx Error" $ ContextsV2.txSignedBy info pkh                   -- the owner must sign it
      ; let c = traceIfFalse "Single Ref In Error" $ isNInputs txRefInputs 1                          -- 1 ref input
      ; let d = traceIfFalse "Datum Continu Error" $ datumAtValidator datum                           -- datum stays the same
      ; let e = traceIfFalse "FT Must Remain"      $ currAmt == nextAmt                               -- datum stays the same
      ;         traceIfFalse "Stable: Mint Error"  $ all (==True) [a,b,c,d,e]
      }
    _  -> False
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    txRefInputs :: [PlutusV2.TxInInfo]
    txRefInputs = PlutusV2.txInfoReferenceInputs info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
  -- find the value at the validator hash
    thisHash :: PlutusV2.ValidatorHash
    thisHash = ContextsV2.ownHash context

    valueAtValidator :: PlutusV2.Value
    valueAtValidator = snd $ head $ ContextsV2.scriptOutputsAt thisHash info

    datumAtValidator :: CustomDatumType -> Bool
    datumAtValidator datum' =
      if length scriptOutputs == 0 
        then False
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> False -- datumless
            (PlutusV2.OutputDatumHash _) -> False -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData @CustomDatumType d of
                Nothing     -> False -- bad data
                Just inline -> datum' == inline
      where 
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt thisHash info

    -- Pay in some token an amount then get an amount back in FT
    convTokenAmount :: Integer -> Bool
    convTokenAmount amount =
      case checkReferenceInputs txRefInputs of
        Nothing     -> False
        Just inline -> do
          { let incToken = Value.singleton (oInPid inline) (oInTkn inline) amount
          ; let relAmt   = getInPriceConversion amount inline
          ; let outToken = Value.singleton (cdtPid datum) (cdtTkn datum) relAmt
          ; let curAmt   = Value.valueOf validatingValue (cdtPid datum) (cdtTkn datum)
          ; let a = traceIfFalse "Bad Cont Value" $ (validatingValue + incToken - outToken) == valueAtValidator
          ; let b = traceIfFalse "Not Enough Tkn" $ curAmt >= relAmt
          ;         traceIfFalse "Value Error"    $ all (==True) [a,b]
          }
          
    -- reference teh oracle stuff
    checkReferenceInputs :: [PlutusV2.TxInInfo] -> Maybe OracleData
    checkReferenceInputs []     = Nothing
    checkReferenceInputs (x:xs) =
      if Value.valueOf (refValue x) oraclePid oracleTkn == 1        -- must hold oracle starter nft
        then
          case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
            PlutusV2.NoOutputDatum       -> checkReferenceInputs xs -- datumless
            (PlutusV2.OutputDatumHash _) -> checkReferenceInputs xs -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData @CognoDatumType d of
                Nothing     -> traceError "cant build from data"    -- bad data
                Just inline -> 
                  case inline of
                    (Oracle od) -> Just od
                    _ -> traceError "cant build non oracle data"
        else checkReferenceInputs xs
      where
        refValue :: PlutusV2.TxInInfo -> PlutusV2.Value
        refValue x' = PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x'
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

marketplaceContractScriptShortBs :: SBS.ShortByteString
marketplaceContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

marketplaceContractScript :: PlutusScript PlutusScriptV2
marketplaceContractScript = PlutusScriptSerialised marketplaceContractScriptShortBs
