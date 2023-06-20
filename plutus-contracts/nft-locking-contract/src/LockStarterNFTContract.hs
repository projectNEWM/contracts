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
module LockStarterNFTContract
  ( lockingContractScript
  , ScriptParameters (..)
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
import qualified UsefulFuncs ( integerAsByteString
                             , isNInputs
                             , isNOutputs
                             , checkValidMultisig
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
  { starterPid :: PlutusV2.CurrencySymbol
  -- ^ Policy ID of the starter token
  , starterTkn :: PlutusV2.TokenName
  -- ^ Token name of the starter token
  , mainPkh    :: PlutusV2.PubKeyHash
  -- ^ The main public key hash for NEWM.
  , multiPkhs  :: [PlutusV2.PubKeyHash]
  -- ^ The multsig public key hashes for NEWM.
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create a token name using a prefix and an integer counter, i.e. token1, token2, etc.
-------------------------------------------------------------------------------
nftName :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
nftName prefix num = prefix <> "_" <> UsefulFuncs.integerAsByteString num
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtNewmPid :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , cdtNumber  :: Integer
  -- ^ The starting number for the catalog.
  , cdtPrefix  :: PlutusV2.BuiltinByteString
  -- ^ The prefix for a catalog.
  }
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]

-- | Short circuit the data comparision with if-then-else statments.
checkDatumIncrease :: CustomDatumType -> CustomDatumType -> Bool
checkDatumIncrease a b =
  if cdtNewmPid a == cdtNewmPid b
    then
      if cdtNumber a + 1 == cdtNumber  b
        then
          if cdtPrefix a == cdtPrefix  b
            then True
            else False
        else False
    else False

-- a is old datum and b is the new datum
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b =  ( cdtNewmPid a == cdtNewmPid b ) &&
            ( cdtNumber  a == cdtNumber  b ) &&
            ( cdtPrefix  a == cdtPrefix  b )
-------------------------------------------------------------------------------
-- | Create the redeemer types.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Mint -- Mint the next sequential tokenization NFT
  | Burn -- Burn a tokenization NFT
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Mint, 0 )
                                                , ( 'Burn, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ScriptParameters -> CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator ScriptParameters {..} datum redeemer context =
  case redeemer of
    -- | Mint a new tokenization NFT with the NEWM main key.
    Mint -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info mainPkh)                       -- NEWM must sign tx
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- Single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- Single script output
         && (traceIfFalse "NFT Minting Error"   checkMintingData)                                           -- Correct token mint
         && (traceIfFalse "Invalid Datum Error" $ isContDatumCorrect contOutputs validatingValue redeemer)  -- Value is continuing and the datum is correct
         && (traceIfFalse "Invalid Starter Tkn" $ Value.valueOf validatingValue starterPid starterTkn == 1) -- Must contain the starter token
    
    -- | Burn any tokenization NFT with the NEWM multisig.
    Burn -> (traceIfFalse "Signing Tx Error"    $ UsefulFuncs.checkValidMultisig info multiPkhs 2)          -- NEWM multisig must sign tx
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- Single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- Single script output
         && (traceIfFalse "NFT Burning Error"   checkBurnedAmount)                                          -- Correct token burn
         && (traceIfFalse "Invalid Datum Error" $ isContDatumCorrect contOutputs validatingValue redeemer)  -- Value is continuing and the datum is correct
         && (traceIfFalse "Invalid Starter Tkn" $ Value.valueOf validatingValue starterPid starterTkn == 1) -- Must contain the starter token
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Validating Value"
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    -- | Check if exactly 1 token is being minted with a specific token name from the datum.
    checkMintingData :: Bool
    checkMintingData =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtNewmPid datum)
                         && (Value.unTokenName tkn == nftName (cdtPrefix datum) (cdtNumber datum))
                         && (amt == (1 :: Integer))
        _                -> traceError "Bad Minting"  -- error on bad mints
    
    -- | Check if exactly 1 token is being burned. Token name here is irrelevant. 
    checkBurnedAmount :: Bool
    checkBurnedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> (cs == cdtNewmPid datum) && (amt == (-1 :: Integer)) -- burn conditions
        _              -> traceError "Bad Burning"                             -- error on bad burns
    
    -- | Check if the continue output datum is of the correct form.
    isContDatumCorrect :: [PlutusV2.TxOut] -> PlutusV2.Value -> CustomRedeemerType -> Bool
    isContDatumCorrect []     _   _         = traceError "Nothing Found"
    isContDatumCorrect (x:xs) val redeemer' =
      if PlutusV2.txOutValue x == val                             -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> traceError "No Datum" -- datumless
            (PlutusV2.OutputDatumHash _) -> traceError "Embedded" -- embedded datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->          -- inline datum only 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Datum"             -- Wrong Data Structure
                Just inline -> 
                  case redeemer' of
                    Mint -> checkDatumIncrease datum inline
                    Burn -> datum == inline
        else isContDatumCorrect xs val redeemer'
-------------------------------------------------------------------------------
-- | Now we need to compile the validator.
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