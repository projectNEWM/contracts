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
module MintFractionalizedTokenContract
  ( mintingPlutusScript
  , mintingScriptShortBs
  , getValidatorHash
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley                                   ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                                       ( serialise )
import qualified Data.ByteString.Lazy                                  as LBS
import qualified Data.ByteString.Short                                 as SBS
import qualified Plutus.V1.Ledger.Scripts                              as Scripts
import qualified Plutus.V1.Ledger.Value                                as Value
import qualified Plutus.V1.Ledger.Address                              as Addr
import qualified Plutus.V2.Ledger.Contexts                             as ContextsV2
import qualified Plutus.V2.Ledger.Api                                  as PlutusV2
import           Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Utils
-- importing only required functions for better readability
import qualified UsefulFuncs ( createBuiltinByteString ) 
{-
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ UsefulFuncs.createBuiltinByteString [96, 138, 16, 184, 220, 94, 217, 124, 13, 104, 193, 47, 161, 33, 111, 96, 128, 124, 146, 60, 200, 10, 128, 93, 132, 230, 149, 191]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
    { crtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , crtTokenizedPid  :: PlutusV2.CurrencySymbol
    -- ^ The Newm tokenized policy id
    , crtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the tokenized token name.
    , crtArtistPKH     :: PlutusV2.PubKeyHash
    -- ^ The artist's public key hash.
    , crtArtistSC      :: PlutusV2.PubKeyHash
    -- ^ The artist's staking key hash.
    }
PlutusTx.unstableMakeIsData ''CustomRedeemerType

instance Eq CustomRedeemerType where
  {-# INLINABLE (==) #-}
  a == b = ( crtFractionalPid a == crtFractionalPid b ) &&
           ( crtTokenizedPid  a == crtTokenizedPid  b ) &&
           ( crtTokenizedTn   a == crtTokenizedTn   b ) &&
           ( crtArtistPKH     a == crtArtistPKH     b ) &&
           ( crtArtistSC      a == crtArtistSC      b )
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy _ context =  (traceIfFalse "Minting/Burning Error" $ (checkMintedAmount && checkInputOutputDatum getValidatorHash) || (checkBurnedAmount && checkInputDatum getValidatorHash))
                   && (traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info getPkh)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    checkPolicyId :: PlutusV2.CurrencySymbol -> Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    mintAmount :: Integer -> Bool
    mintAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (100_000_000 :: Integer)
    
    burnAmount :: Integer -> Bool
    burnAmount amt = traceIfFalse "Incorrect Burn Amount" $ amt == (-100_000_000 :: Integer)

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && mintAmount amt
        _              -> traceIfFalse "Minting Error" False

    checkBurnedAmount :: Bool
    checkBurnedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && burnAmount amt
        _              -> traceIfFalse "Burning Error" False
    

    -- check if the incoming datum is the correct form.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomRedeemerType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        

    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> PlutusV2.ValidatorHash -> Maybe CustomRedeemerType
    checkInputs []     _     = Nothing
    checkInputs (x:xs) vHash =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress vHash
      then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
      else checkInputs xs vHash
    
    datumAtValidator :: Maybe CustomRedeemerType
    datumAtValidator = 
      if length scriptOutputs == 0 
        then Nothing
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> Nothing -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
      where
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt getValidatorHash info

    -- check that the locking script has the correct datum hash
    checkInputDatum :: PlutusV2.ValidatorHash -> Bool
    checkInputDatum vHash =
      case checkInputs txInputs vHash of
        Nothing -> traceError "No Input Datum"
        Just _  -> True
    
    -- check that the locking script has the correct datum hash
    checkInputOutputDatum :: PlutusV2.ValidatorHash -> Bool
    checkInputOutputDatum vHash =
      case checkInputs txInputs vHash of
        Nothing         -> traceError "No Input Datum"
        Just inputDatum ->
          case datumAtValidator of
            Nothing          -> traceError "No Output Datum"
            Just outputDatum -> inputDatum == outputDatum
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