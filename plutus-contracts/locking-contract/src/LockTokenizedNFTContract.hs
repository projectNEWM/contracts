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
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module LockTokenizedNFTContract
  ( lockingContractScript
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley       ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V2.Ledger.Contexts as ContextsV2
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified Plutonomy
-- importing only required functions for better readability
import qualified UsefulFuncs ( isNInputs
                             , isNOutputs
                             )
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { tPid     :: PlutusV2.CurrencySymbol
  -- ^ Newm's tokenization policy id
  , mainPkh :: PlutusV2.PubKeyHash
  -- ^ The main public key hash for NEWM.
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , cdtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the artist's tokenized token name.
    }
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]

-- a is old datum and b is the new datum
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtFractionalPid a == cdtFractionalPid b ) &&
           ( cdtTokenizedTn   a == cdtTokenizedTn   b )
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Lock   -- Lock and fractionalize the NFT
  | Unlock -- Unlock and solidify the fractions, releasing the nft
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Lock,   0 )
                                                , ( 'Unlock, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ScriptParameters -> CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator ScriptParameters {..} datum redeemer context =
  case redeemer of
    -- | Lock Tokenized Token into contract and fractionalized
    Lock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info mainPkh)                       -- newm signs it
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- single script output
         && (traceIfFalse "Minting Error"       checkMintedAmount)                                          -- mint the ft only
         && (traceIfFalse "Invalid Datum Error" $ isDatumConstant contOutputs validatingValue singularNFT)  -- value is cont and the datum is correct.

    -- | Unlock Tokenized Token from contract by solidifying the fractional tokens.
    Unlock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info mainPkh)                             -- newm signs it
           && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                               -- single script input
           && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 0)                           -- single script output
           && (traceIfFalse "Burning Error"        checkMintedAmount)                                               -- burn the ft only
           && (traceIfFalse "Invalid Token Error" $ Value.valueOf validatingValue tPid (cdtTokenizedTn datum) == 1) -- Must contain the starter token
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    -- | This is the currently validating value from the UTxO being spent in this tx.
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate"
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- | This is the NFT coming into the contract to be fractionalized.
    singularNFT :: PlutusV2.Value
    singularNFT = Value.singleton tPid (cdtTokenizedTn datum) (1 :: Integer)

    -- | Check if the currency symbol and token name are correct for fractionalize. Amount is handled in MintFractionalizedTokenContract.
    checkMintedAmount :: Bool
    checkMintedAmount = 
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> cs == cdtFractionalPid datum  -- Must be the MintFractionalizedTokenContract currency symbol
                      && tn == cdtTokenizedTn datum    -- Must be the correct token name from datum
        _             -> traceError "Mint/Burn Error"
    
    -- | Check if the continue output datum remains constant inside the tx.
    isDatumConstant :: [PlutusV2.TxOut] -> PlutusV2.Value -> PlutusV2.Value -> Bool
    isDatumConstant []     _   _   = traceError "Nothing Found"
    isDatumConstant (x:xs) val nft =
      if PlutusV2.txOutValue x == (val + nft)                      -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> traceError "No Datum"  -- datumless
            (PlutusV2.OutputDatumHash _) -> traceError "Embedded"  -- embedded datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->           -- inline datum only
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Datum"              -- Bad Data
                Just inline -> datum == inline
        else isDatumConstant xs val nft
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedValidator :: ScriptParameters -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator s x y z = check (mkValidator s (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))

validator :: ScriptParameters -> PlutusV2.Validator
validator sp = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
  $$(PlutusTx.compile [|| wrappedValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

lockingContractScript :: ScriptParameters -> PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . validator