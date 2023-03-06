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
{-# LANGUAGE RecordWildCards       #-}
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
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley       ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Address  as Addr
import qualified Plutus.V2.Ledger.Contexts as ContextsV2
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified Plutonomy
{-
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { mainPkh    :: PlutusV2.PubKeyHash
  -- ^ The main public key hash for NEWM.
  , validatorHash :: PlutusV2.ValidatorHash
  -- ^ The LockTokenizedNFTContract validator hash.
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , cdtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the tokenized token name.
    }
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]

instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtFractionalPid a == cdtFractionalPid b ) &&
           ( cdtTokenizedTn   a == cdtTokenizedTn   b )
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptParameters -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy ScriptParameters {..} _ context =  (traceIfFalse "Signing Tx Error" $ ContextsV2.txSignedBy info mainPkh)
                                         && (traceIfFalse "Mint/Burn/Datum Error"  $ (checkMintedAmount && checkInputOutputDatum validatorHash) || (checkBurnedAmount && checkInputDatum validatorHash))
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    checkPolicyId :: PlutusV2.CurrencySymbol -> Bool
    checkPolicyId cs = cs == ContextsV2.ownCurrencySymbol context

    mintAmount :: Integer -> Bool
    mintAmount amt = amt == (100_000_000 :: Integer)
    
    burnAmount :: Integer -> Bool
    burnAmount amt = amt == (-100_000_000 :: Integer)

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
        scriptOutputs = ContextsV2.scriptOutputsAt validatorHash info

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
            Nothing          -> False
            Just outputDatum -> inputDatum == outputDatum
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedPolicy :: ScriptParameters -> BuiltinData -> BuiltinData -> ()
wrappedPolicy s x y = check (mkPolicy s (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y))

policy :: ScriptParameters -> PlutusV2.MintingPolicy
policy sp = PlutusV2.mkMintingPolicyScript $ 
  $$(PlutusTx.compile [|| wrappedPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

mintingPlutusScript :: ScriptParameters -> PlutusScript PlutusScriptV2
mintingPlutusScript sp = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict $ serialise $ Plutonomy.optimizeUPLC $ PlutusV2.Validator $ PlutusV2.unMintingPolicyScript (policy sp)