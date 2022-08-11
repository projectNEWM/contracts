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
module NFTLockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  , CustomDatumType
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
import           CheckFuncs
import           TokenHelper
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}

lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [164, 41, 115, 189, 213, 27, 58, 248, 178, 206, 124, 143, 246, 58, 68, 143, 212, 246, 40, 205, 78, 231, 162, 174, 187, 234, 169, 3] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [84, 104, 101, 80, 114, 111, 106, 101, 99, 116, 78, 101, 119, 77, 83, 116, 97, 114, 116, 101, 114, 84, 111, 107, 101, 110] }

-- check for nft here
lockValue :: PlutusV2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)

-- main key
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

-- collat wallet
multiPkh1 :: PlutusV2.PubKeyHash
multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [139, 28, 31, 194, 168, 74, 122, 168, 84, 106, 79, 32, 249, 214, 245, 237, 203, 6, 229, 147, 38, 54, 136, 116, 43, 119, 78, 57] }
-- seller wallet
multiPkh2 :: PlutusV2.PubKeyHash
multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }
-- reference wallet
multiPkh3 :: PlutusV2.PubKeyHash
multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [201, 200, 26, 235, 56, 208, 42, 163, 75, 112, 228, 42, 144, 232, 132, 53, 167, 41, 234, 98, 210, 75, 30, 174, 237, 246, 142, 9] }

-- all possible signers
listOfPkh :: [PlutusV2.PubKeyHash]
listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
-------------------------------------------------------------------------------
-- | Simple Multisig
-------------------------------------------------------------------------------
checkMultisig :: PlutusV2.TxInfo -> [PlutusV2.PubKeyHash] -> Integer -> Bool
checkMultisig txInfo pkhs amt = loopSigs pkhs 0
  where
    loopSigs :: [PlutusV2.PubKeyHash] -> Integer  -> Bool
    loopSigs []     counter = counter >= amt
    loopSigs (x:xs) counter = 
      if ContextsV2.txSignedBy txInfo x
        then loopSigs xs (counter + 1)
        else loopSigs xs counter
-------------------------------------------------------------------------------
-- | A custom eq class for datum objects.
-------------------------------------------------------------------------------
class Equiv a where
  (===) :: a -> a -> Bool
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
PlutusTx.unstableMakeIsData ''CustomDatumType

-- old == new | minting
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtNewmPid    a == cdtNewmPid b ) &&
           ( cdtNumber a + 1 == cdtNumber  b ) &&
           ( cdtPrefix     a == cdtPrefix  b )
-- old === new | burning
instance Equiv CustomDatumType where
  {-# INLINABLE (===) #-}
  a === b = ( cdtNewmPid a == cdtNewmPid b ) &&
            ( cdtNumber  a == cdtNumber  b ) &&
            ( cdtPrefix  a == cdtPrefix  b )
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Mint |
                          Burn |
                          Exit -- remove in production
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Mint, 0 )
                                                , ( 'Burn, 1 )
                                                , ( 'Exit, 2 ) -- remove in production
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Mint -> do
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh                -- newm signs it
      ; let b = traceIfFalse "Single In/Out Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let c = traceIfFalse "NFT Minting Error"   checkMintedAmount                                  -- mint an nft only
      ; let d = traceIfFalse "Invalid Datum Error" $ isEmbeddedDatumIncreasing contOutputs            -- value is cont and the datum is correct.
      ; let e = traceIfFalse "Invalid Start Token" $ Value.geq validatingValue lockValue              -- must contain the start token
      ;         traceIfFalse "Locking:Mint Error"  $ all (==True) [a,b,c,d,e]
      }
    Burn -> do
      { let a = traceIfFalse "Signing Tx Error"      $ checkMultisig info listOfPkh 2                   -- newm multisig
      ; let b = traceIfFalse "Single In/Out Error"   $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let c = traceIfFalse "NFT Burning Error"     checkBurnedAmount                                  -- burn an nft only
      ; let d = traceIfFalse "Invalid Datum Error"   $ isEmbeddedDatumConstant contOutputs              -- value is cont and the datum is correct.
      ; let e = traceIfFalse "Invalid Starter Token" $ Value.geq validatingValue lockValue              -- must contain the start token
      ;         traceIfFalse "Locking:Burn Error"    $ all (==True) [a,b,c,d,e]
      }
    Exit -> do -- remove in production
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh
      ;         traceIfFalse "Exit Endpoint Error" $ all (==True) [a]
      }
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    -- minting stuff
    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtNewmPid datum) && (Value.unTokenName tkn == nftName (cdtPrefix datum) (cdtNumber datum)) && (amt == (1 :: Integer))
        _                -> False
    
    -- burning stuff
    checkBurnedAmount :: Bool
    checkBurnedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> (cs == cdtNewmPid datum) && (amt == (-1 :: Integer))
        _              -> False
    
    -- datum stuff
    isEmbeddedDatumIncreasing :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumIncreasing []     = traceIfFalse "No Datum Found" False
    isEmbeddedDatumIncreasing (x:xs) =
      if PlutusV2.txOutValue x == validatingValue -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            -- datumless
            PlutusV2.NoOutputDatum -> isEmbeddedDatumIncreasing xs
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> isEmbeddedDatumIncreasing xs
                Just inline -> datum == inline
            -- embedded datum
            (PlutusV2.OutputDatumHash dh) -> 
              case ContextsV2.findDatum dh info of
                Nothing                  -> isEmbeddedDatumIncreasing xs
                Just (PlutusV2.Datum d') -> 
                  case PlutusTx.fromBuiltinData d' of
                    Nothing       -> isEmbeddedDatumIncreasing xs
                    Just embedded -> datum == embedded
        else isEmbeddedDatumIncreasing xs

    -- datum stuff
    isEmbeddedDatumConstant :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumConstant []     = traceIfFalse "No Datum Found" False
    isEmbeddedDatumConstant (x:xs) =
      if PlutusV2.txOutValue x == validatingValue -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            -- datumless
            PlutusV2.NoOutputDatum -> isEmbeddedDatumConstant xs
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> isEmbeddedDatumConstant xs
                Just inline -> datum === inline
            -- embedded datum
            (PlutusV2.OutputDatumHash dh) -> 
              case ContextsV2.findDatum dh info of
                Nothing                  -> isEmbeddedDatumConstant xs
                Just (PlutusV2.Datum d') -> 
                  case PlutusTx.fromBuiltinData d' of
                    Nothing       -> isEmbeddedDatumConstant xs
                    Just embedded -> datum === embedded
        else isEmbeddedDatumConstant xs

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

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs