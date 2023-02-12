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
module LockStarterNFTContract
  ( lockingContractScript
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
import qualified UsefulFuncs ( createBuiltinByteString
                             , integerAsByteString
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
-- | The starter token information.
-------------------------------------------------------------------------------
-- CurrencySymbol for the starter token.
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [38, 144, 61, 231, 221, 148, 253, 203, 89, 253, 43, 89, 128, 168, 202, 79, 247, 31, 6, 47, 126, 210, 88, 89, 203, 38, 232, 127] }
-- TokenName for the starter token.
lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = UsefulFuncs.createBuiltinByteString [78, 69, 87, 77, 95] }
-- Value for the starter token.
lockValue :: PlutusV2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)
-------------------------------------------------------------------------------
-- | The main public key hash for NEWM.
-------------------------------------------------------------------------------
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }
-------------------------------------------------------------------------------
-- | Hardcoded multisig participants.
-------------------------------------------------------------------------------
-- wallet 1
multiPkh1 :: PlutusV2.PubKeyHash
multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [150, 147, 223, 102, 202, 166, 174, 17, 93, 95, 24, 126, 236, 103, 146, 36, 158, 100, 86, 102, 7, 76, 76, 77, 115, 247, 147, 132] }
-- wallet 2
multiPkh2 :: PlutusV2.PubKeyHash
multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [213, 247, 65, 6, 15, 203, 170, 116, 238, 77, 158, 69, 116, 252, 176, 72, 211, 197, 56, 78, 192, 206, 73, 158, 8, 137, 190, 83] }
-- wallet 3
multiPkh3 :: PlutusV2.PubKeyHash
multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [67, 158, 82, 1, 141, 168, 20, 19, 240, 146, 132, 217, 97, 51, 160, 89, 193, 4, 222, 70, 42, 11, 29, 37, 211, 114, 106, 151] }
-------------------------------------------------------------------------------
-- | All possible signers inside the multisig
-------------------------------------------------------------------------------
listOfPkh :: [PlutusV2.PubKeyHash]
listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
-------------------------------------------------------------------------------
-- | Create a token name using a prefix and an integer counter, i.e. token1, token2, etc.
-------------------------------------------------------------------------------
nftName :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
nftName prefix num = prefix <> UsefulFuncs.integerAsByteString num
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
data CustomRedeemerType = Mint |
                          Burn
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Mint, 0 )
                                                , ( 'Burn, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    -- | Mint a new tokenization NFT with the NEWM main key.
    Mint -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh)                        -- NEWM must sign tx
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- Single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- Single script output
         && (traceIfFalse "NFT Minting Error"   checkMintingData)                                           -- Correct token mint
         && (traceIfFalse "Invalid Datum Error" $ isContDatumCorrect contOutputs validatingValue redeemer)  -- Value is continuing and the datum is correct
         && (traceIfFalse "Invalid Mint Error" $ Value.geq validatingValue lockValue)                      -- Must contain the starter token
    
    -- | Burn any tokenization NFT with the NEWM multisig.
    Burn -> (traceIfFalse "Signing Tx Error"    $ UsefulFuncs.checkValidMultisig info listOfPkh 2)          -- NEWM multisig must sign tx
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- Single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- Single script output
         && (traceIfFalse "NFT Burning Error"   checkBurnedAmount)                                          -- Correct token burn
         && (traceIfFalse "Invalid Datum Error" $ isContDatumCorrect contOutputs validatingValue redeemer)  -- Value is continuing and the datum is correct
         && (traceIfFalse "Invalid Burn Error" $ Value.geq validatingValue lockValue)                      -- Must contain the starter token
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

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

    -- | Check if exactly 1 token is being minted with a specific token name from the datum.
    checkMintingData :: Bool
    checkMintingData =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtNewmPid datum)
                         && (Value.unTokenName tkn == nftName (cdtPrefix datum) (cdtNumber datum))
                         && (amt == (1 :: Integer))
        _                -> traceError "Bad Minting Data"  -- error on bad mints
    
    -- | Check if exactly 1 token is being burned. Token name here is irrelevant. 
    checkBurnedAmount :: Bool
    checkBurnedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> (cs == cdtNewmPid datum) && (amt == (-1 :: Integer))
        _              -> traceError "Bad Burning Data"  -- error on bad burns
    
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
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))

validator :: PlutusV2.Validator
validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
-- validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs