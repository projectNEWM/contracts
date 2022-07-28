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
module V2NFTLockingContract
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
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           V2CheckFuncs
import           V2TokenHelper
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2

import binascii
a="a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439"
s=binascii.unhexlify(a)
[x for x in s]
-}
{-# INLINABLE getPkh #-}
getPkh :: PlutusV2.PubKeyHash -- remove in production
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }

voteValidatorHash :: PlutusV2.ValidatorHash
voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [219, 86, 62, 255, 32, 206, 47, 101, 6, 30, 203, 98, 144, 37, 92, 85, 71, 12, 57, 76, 63, 69, 147, 89, 196, 76, 126, 72]

votePid :: PlutusV2.CurrencySymbol
votePid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString []}

voteTkn :: PlutusV2.TokenName
voteTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString []}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data VoteDatumType = VoteDatumType
    { vdtPid :: PlutusV2.CurrencySymbol
    -- ^ The voting token's policy id
    , vdtTkn :: PlutusV2.TokenName
    -- ^ The voting token's token name.
    , vdtAmt :: Integer
    -- ^ The voting token's threshold amount.
    }
PlutusTx.unstableMakeIsData ''VoteDatumType
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
data CustomRedeemerType = MintNFT |
                          BurnNFT |
                          Exit
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'MintNFT, 0 )
                                                , ( 'BurnNFT, 1 )
                                                , ( 'Exit,    2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    MintNFT -> do
      { let a = traceIfFalse "Vote Has Failed"            $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error"        $ isNScripts txInputs 1 && isNScripts txRefInputs 1
      ; let c = traceIfFalse "Cont Payin Error"           $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "NFT Minting Error"          checkMintedAmount
      ; let e = traceIfFalse "Datum Not Increasing Error" $ isEmbeddedDatumIncreasing contOutputs
      ;         traceIfFalse "Locking Contract Mint Endpoint Error" $ all (==True) [a,b,c,d,e]
      }
    BurnNFT -> do
      { let a = traceIfFalse "Vote Has Failed"          $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error"      $ isNScripts txInputs 1 && isNScripts txRefInputs 1
      ; let c = traceIfFalse "Cont Payin Error"         $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "NFT Burning Error"        checkBurnedAmount
      ; let e = traceIfFalse "Datum Not Constant Error" $ isEmbeddedDatumConstant contOutputs
      ;         traceIfFalse "Locking Contract Burn Endpoint Error" $ all (==True) [a,b,c,d,e]
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
    txInputs = PlutusV2.txInfoInputs info
    
    txRefInputs :: [PlutusV2.TxInInfo]
    txRefInputs = PlutusV2.txInfoReferenceInputs info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtNewmPid datum) && (Value.unTokenName tkn == nftName (cdtPrefix datum) (cdtNumber datum)) && (amt == (1 :: Integer))
        _                -> False
    
    checkBurnedAmount :: Bool
    checkBurnedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> (cs == cdtNewmPid datum) && (amt == (-1 :: Integer))
        _              -> False
    
    -- check for nft here
    voteStartValue :: PlutusV2.Value
    voteStartValue = Value.singleton votePid voteTkn (1 :: Integer)

    checkVoteFromDatum :: [PlutusV2.TxInInfo] -> Bool
    checkVoteFromDatum []     = traceIfFalse "No Datum Found on Reference Input" False
    checkVoteFromDatum (x:xs) =
      if traceIfFalse "Incorrect Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress voteValidatorHash
        then
          if traceIfFalse "Incorrect Value" $ Value.geq (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) voteStartValue
            then
              case getReferenceDatum $ PlutusV2.txInInfoResolved x of
                Nothing -> checkVoteFromDatum xs
                Just voteDatum -> isVoteComplete (vdtPid voteDatum) (vdtTkn voteDatum) (vdtAmt voteDatum) info
            else checkVoteFromDatum xs
        else checkVoteFromDatum xs

    getReferenceDatum :: PlutusV2.TxOut -> Maybe VoteDatumType
    getReferenceDatum x =
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> Nothing
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> --Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType d
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> Nothing
            Just (PlutusV2.Datum d') -> --Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType d'
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> Nothing
                Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType embedded


    isEmbeddedDatumIncreasing :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumIncreasing []     = False
    isEmbeddedDatumIncreasing (x:xs) =
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

    
    isEmbeddedDatumConstant :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumConstant []     = False
    isEmbeddedDatumConstant (x:xs) =
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