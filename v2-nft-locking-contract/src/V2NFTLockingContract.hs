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
voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [45, 120, 123, 140, 75, 245, 182, 82, 209, 137, 32, 129, 200, 229, 84, 212, 13, 59, 140, 156, 7, 94, 119, 178, 248, 236, 19, 215]

-- voting starter token
voteStartPid :: PlutusV2.CurrencySymbol
voteStartPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [152, 47, 147, 160, 239, 222, 142, 221, 14, 154, 244, 0, 218, 8, 62, 145, 217, 142, 29, 91, 74, 119, 160, 121, 56, 164, 222, 79] }

voteStartTkn :: PlutusV2.TokenName
voteStartTkn = PlutusV2.TokenName { PlutusV2.unTokenName = createBuiltinByteString [116, 104, 105, 115, 105, 115, 97, 118, 101, 114, 121, 108, 111, 110, 103, 115, 116, 114, 105, 110, 103, 102, 111, 114, 116, 101, 115, 116, 105, 110, 49, 48] }

voteStartValue :: PlutusV2.Value
voteStartValue = Value.singleton voteStartPid voteStartTkn (1 :: Integer)

-- incoming token to start the contract
lockStartPid :: PlutusV2.CurrencySymbol
lockStartPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [16, 65, 164, 127, 35, 117, 73, 14, 129, 240, 107, 173, 180, 173, 42, 184, 84, 104, 176, 202, 144, 235, 43, 66, 87, 224, 180, 154] }

lockStartTkn :: PlutusV2.TokenName
lockStartTkn = PlutusV2.TokenName { PlutusV2.unTokenName = createBuiltinByteString [84, 111, 107, 104, 117, 110, 83, 109, 97, 114, 116, 67, 111, 110, 116, 114, 97, 99, 116, 48, 48, 49] }

lockStartValue :: PlutusV2.Value
lockStartValue = Value.singleton lockStartPid lockStartTkn (1 :: Integer)
-------------------------------------------------------------------------------
-- | Create the delegation parameters data object.
-------------------------------------------------------------------------------
data DelegationType = DelegationType
    { dtPkh    :: PlutusV2.PubKeyHash
    -- ^ The did public key hash.
    , dtIouPid :: PlutusV2.CurrencySymbol
    -- ^ The dids iou policy id.
    , dtHash   :: PlutusV2.ValidatorHash
    -- ^ The dids iou policy id.
    }
PlutusTx.unstableMakeIsData ''DelegationType
-------------------------------------------------------------------------------
-- | Create the voting datum parameters data object.
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
      { let a = traceIfFalse "Vote Has Failed"     $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error" $ isNScripts txInputs 1
      ; let c = traceIfFalse "Cont Payin Error"    $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "NFT Minting Error"   checkMintedAmount
      ; let e = traceIfFalse "Wrong Datum Error"   $ isEmbeddedDatumIncreasing contOutputs
      ; let f = traceIfFalse "Wrong Starter Error" $ Value.geq validatingValue lockStartValue
      ;         traceIfFalse "Locking Mint Error"  $ all (==True) [a,b,c,d,e,f]
      }
    BurnNFT -> do
      { let a = traceIfFalse "Vote Has Failed"     $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error" $ isNScripts txInputs 1
      ; let c = traceIfFalse "Cont Payin Error"    $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "NFT Burning Error"   checkBurnedAmount
      ; let e = traceIfFalse "Wrong Datum Error"   $ isEmbeddedDatumConstant contOutputs
      ; let f = traceIfFalse "Wrong Starter Error" $ Value.geq validatingValue lockStartValue
      ;         traceIfFalse "Locking Burn Error"  $ all (==True) [a,b,c,d,e,f]
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

    checkVoteFromDatum :: [PlutusV2.TxInInfo] -> Bool
    checkVoteFromDatum []     = traceIfFalse "No Datum Found on Reference Input" False
    checkVoteFromDatum (x:xs) =
      if traceIfFalse "Incorrect Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress voteValidatorHash
        then
          if traceIfFalse "Incorrect Value" $ Value.geq (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) voteStartValue
            then
              case getReferenceDatum $ PlutusV2.txInInfoResolved x of
                Nothing -> checkVoteFromDatum xs
                Just voteDatum' -> checkReferenceSigners voteDatum' txRefInputs (Value.singleton Value.adaSymbol Value.adaToken (0 :: Integer)) -- isVoteComplete (vdtPid voteDatum) (vdtTkn voteDatum) (vdtAmt voteDatum) info -- this becomes the ref call
            else checkVoteFromDatum xs
        else checkVoteFromDatum xs

    -- check if the outgoing datum has the correct form.
    checkReferenceSigners :: VoteDatumType -> [PlutusV2.TxInInfo] -> PlutusV2.Value -> Bool
    checkReferenceSigners voteDatum []     val = isVoteComplete (vdtPid voteDatum) (vdtTkn voteDatum) (vdtAmt voteDatum) info val -- input the voting token info
    checkReferenceSigners voteDatum (x:xs) val = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        -- datumless
        PlutusV2.NoOutputDatum -> checkReferenceSigners voteDatum xs val
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData @DelegationType d of
            Nothing     -> checkReferenceSigners voteDatum xs val
            Just inline -> 
              if traceIfFalse "Incorrect Validator Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress (dtHash inline)
              then ContextsV2.txSignedBy info (dtPkh inline) && checkReferenceSigners voteDatum xs (val + (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x))
              else checkReferenceSigners voteDatum xs val
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) ->
          case ContextsV2.findDatum dh info of
            Nothing                  -> checkReferenceSigners voteDatum xs val
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData @DelegationType d' of
                Nothing       -> checkReferenceSigners voteDatum xs val
                Just embedded -> 
                  if traceIfFalse "Incorrect Validator Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress (dtHash embedded)
                  then ContextsV2.txSignedBy info (dtPkh embedded) && checkReferenceSigners voteDatum xs (val + (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x))
                  else checkReferenceSigners voteDatum xs val


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


    -- get the equality instances
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