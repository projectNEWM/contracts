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
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley       ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Address  as Addr
import qualified Plutus.V2.Ledger.Contexts as ContextsV2
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified Plutonomy
-- importing only required functions for better readability
import qualified UsefulFuncs ( createBuiltinByteString ) 
{-
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
-------------------------------------------------------------------------------
-- | The main public key hash for NEWM.
-------------------------------------------------------------------------------
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }
-------------------------------------------------------------------------------
-- | The validator hash of the LockTokenizedNFTContract.
-------------------------------------------------------------------------------
getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ UsefulFuncs.createBuiltinByteString [112, 254, 182, 23, 186, 182, 65, 199, 92, 8, 145, 191, 173, 199, 164, 98, 180, 103, 212, 161, 244, 202, 136, 193, 203, 84, 59, 90]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , cdtTokenizedPid  :: PlutusV2.CurrencySymbol
    -- ^ The Newm tokenized policy id
    , cdtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the tokenized token name.
    }
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]

instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtFractionalPid a == cdtFractionalPid b ) &&
           ( cdtTokenizedPid  a == cdtTokenizedPid  b ) &&
           ( cdtTokenizedTn   a == cdtTokenizedTn   b )
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

    -- | Return the inline datum from a tx out.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomDatumType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing       -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing       -- embedded datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->  -- inline datum
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing                    -- Bad Data
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        

    -- | Return the first inline datum from the LockStarterNFTContract from the list of inputs.
    checkInputs :: [PlutusV2.TxInInfo] -> PlutusV2.ValidatorHash -> Maybe CustomDatumType
    checkInputs []     _     = Nothing
    checkInputs (x:xs) vHash =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress vHash
        then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
        else checkInputs xs vHash
    
    -- | Get the datum on the output going back to the LockTokenizedNFTContract.
    datumAtValidator :: Maybe CustomDatumType
    datumAtValidator = 
      if length scriptOutputs == 0                        -- Prevent head of empty list error
        then Nothing
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> Nothing       -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing       -- embedded datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->  -- inline datum
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing                    -- Bad Data
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
      where
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt getValidatorHash info

    -- | Check that a datum on the input from the LockTokenizedNFTContract is being spent.
    checkInputDatum :: PlutusV2.ValidatorHash -> Bool
    checkInputDatum vHash =
      case checkInputs txInputs vHash of
        Nothing -> traceIfFalse "No Input Datum" False
        Just _  -> True
    
    -- | Check that a datum on the input from the LockTokenizedNFTContract is equal to the datum on the output to the LockTokenizedNFTContract.
    checkInputOutputDatum :: PlutusV2.ValidatorHash -> Bool
    checkInputOutputDatum vHash =
      case checkInputs txInputs vHash of
        Nothing         -> traceError "No Input Datum"
        Just inputDatum ->
          case datumAtValidator of
            Nothing          -> traceIfFalse "No Output Datum" False
            Just outputDatum -> inputDatum == outputDatum
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedPolicy :: BuiltinData -> BuiltinData -> ()
wrappedPolicy x y = check (mkPolicy (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y))

policy :: PlutusV2.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| wrappedPolicy ||])

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise $ Plutonomy.optimizeUPLC $ validator
-- scriptAsCbor = serialise $ Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor