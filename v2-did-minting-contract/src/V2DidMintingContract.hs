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
module V2DidMintingContract
  ( didMintingPlutusScript
  , didMintingScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
flattenBuiltinByteString [] = emptyByteString 
flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)

{-# INLINABLE createBuiltinByteString #-}
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString |x <- intList ]

-- the hash of the locking script
lockValidatorHash :: PlutusV2.ValidatorHash
lockValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [115, 164, 56, 226, 128, 13, 53, 211, 159, 154, 146, 97, 232, 133, 78, 251, 19, 210, 246, 171, 61, 24, 149, 68, 26, 68, 235, 42]

-- must match a specific pkh
delegatorPkh :: PlutusV2.PubKeyHash
delegatorPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }

-- constant token name
iouTkn :: PlutusV2.TokenName
iouTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [105, 111, 117]}
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
    { crtPkh :: PlutusV2.PubKeyHash
    -- ^ The did public key hash.
    }
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-- old == new
instance Eq CustomRedeemerType where
  {-# INLINABLE (==) #-}
  a == b = ( crtPkh a == crtPkh b )
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy _ context = traceIfFalse "Mint/Burn Error" $ checkMintingProcess && checkInputDatum && checkOutputDatum
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- the redeemer is the datum of the locking script
    -- redeemer :: CustomRedeemerType
    -- redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'

    -- check if the incoming datum is the correct form.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomRedeemerType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> Nothing
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> Nothing
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> Nothing
                Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType embedded
    
    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> Maybe CustomRedeemerType
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress lockValidatorHash
      then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
      else checkInputs xs

    -- check that the locking script has the correct datum hash
    checkInputDatum :: Bool
    checkInputDatum =
      case checkInputs txInputs of
        Nothing         -> traceIfFalse "No Input Datum Error" False
        Just inputDatum -> traceIfFalse "Input Datum Equality Error" $ crtPkh inputDatum == delegatorPkh
    
    datumAtValidator :: Maybe CustomRedeemerType
    datumAtValidator =
      if checkScriptOutputs $ ContextsV2.scriptOutputsAt lockValidatorHash info
        then Nothing
        else
          case datumAtValidator' of
            -- datumless
            PlutusV2.NoOutputDatum -> Nothing
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
            -- embedded datum
            (PlutusV2.OutputDatumHash dh) -> 
              case ContextsV2.findDatum dh info of
                Nothing                  -> Nothing
                Just (PlutusV2.Datum d') -> 
                  case PlutusTx.fromBuiltinData d' of
                    Nothing       -> Nothing
                    Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType embedded
      where
        checkScriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)] -> Bool
        checkScriptOutputs scriptOutputs' = loopInputs scriptOutputs' 0
          where
            loopInputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)] -> Integer -> Bool
            loopInputs [] counter = counter == 0
            loopInputs _  _       = False
        
        datumAtValidator' :: PlutusV2.OutputDatum
        datumAtValidator' = fst $ head $ ContextsV2.scriptOutputsAt lockValidatorHash info

    -- the output datum for minting increases the number by one
    checkOutputDatum :: Bool
    checkOutputDatum = 
      case datumAtValidator of
        Nothing      -> traceIfFalse "No Datum At Validator" False
        Just datum'' -> traceIfFalse "Output Datum Equality Error" $ crtPkh datum'' == delegatorPkh

    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkTokenName :: PlutusV2.TokenName -> Bool
    checkTokenName tn = traceIfFalse "Incorrect Token Name" $ tn == iouTkn

    checkMintingProcess :: Bool
    checkMintingProcess =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> checkPolicyId cs && checkTokenName tn
        _               -> traceIfFalse "Nothing Is Minting" False
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
policy :: PlutusV2.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Utils.mkUntypedMintingPolicy mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

didMintingPlutusScript :: PlutusScript PlutusScriptV2
didMintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

didMintingScriptShortBs :: SBS.ShortByteString
didMintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor