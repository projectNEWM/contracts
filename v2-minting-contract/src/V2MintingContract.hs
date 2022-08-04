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
module V2MintingContract
  ( mintingPlutusScript
  , mintingScriptShortBs
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

lockValidatorHash :: PlutusV2.ValidatorHash
lockValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [80, 34, 119, 245, 231, 72, 90, 200, 117, 16, 89, 211, 246, 128, 185, 214, 113, 50, 167, 17, 18, 159, 209, 215, 96, 28, 140, 105]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
    { crtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , crtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the tokenized token name.
    , crtArtistPKH     :: PlutusV2.PubKeyHash
    -- ^ The artist's public key hash.
    , crtArtistSC      :: PlutusV2.PubKeyHash
    -- ^ The artist's staking key hash.
    }
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-- old == new
instance Eq CustomRedeemerType where
  {-# INLINABLE (==) #-}
  a == b = ( crtFractionalPid a == crtFractionalPid b ) &&
           ( crtTokenizedTn   a == crtTokenizedTn   b ) &&
           ( crtArtistPKH     a == crtArtistPKH     b ) &&
           ( crtArtistSC      a == crtArtistSC      b )
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer' context = traceIfFalse "Mint/Burn Error" ((checkMintingProcess && checkInputDatum && checkOutputDatum) || (checkBurningProcess && checkInputDatum))
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- the redeemer is the datum of the locking script
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'

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
        Just inputDatum -> traceIfFalse "Input Datum Equality Error" $ inputDatum == redeemer
    
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
        Just datum'' -> traceIfFalse "Output Datum Equality Error" $ datum'' == redeemer

    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkTokenName :: PlutusV2.TokenName -> Bool
    checkTokenName tn = traceIfFalse "Incorrect Token Name" $ tn == crtTokenizedTn redeemer

    checkMintingProcess :: Bool
    checkMintingProcess =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, amt)] -> checkPolicyId cs && checkTokenName tn && (traceIfFalse "Mint Amount Error" $ amt == (100000000 :: Integer))
        _               -> traceIfFalse "Nothing Is Minting" False
    
    checkBurningProcess :: Bool
    checkBurningProcess =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, amt)] -> checkPolicyId cs && checkTokenName tn && (traceIfFalse "Burn Amount Error" $ amt == (-100000000 :: Integer))
        _               -> traceIfFalse "Nothing Is Burning" False
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

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor