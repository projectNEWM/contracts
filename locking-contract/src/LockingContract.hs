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
module LockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  , CustomDatumType
  , getPkh
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
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

-- tokenization minting policy
tokenizedPid :: PlutusV2.CurrencySymbol
tokenizedPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [249, 53, 233, 120, 58, 60, 119, 165, 234, 123, 221, 24, 231, 194, 12, 246, 98, 146, 119, 1, 34, 191, 232, 111, 170, 88, 58, 253] }


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , cdtTokenizedPid  :: PlutusV2.CurrencySymbol
    -- ^ The artist's tokenized policy id
    , cdtTokenizedTn   :: PlutusV2.TokenName
    -- ^ the artist's tokenized token name.
    , cdtArtistPKH     :: PlutusV2.PubKeyHash
    -- ^ The artist's public key hash.
    , cdtArtistSC      :: PlutusV2.PubKeyHash
    -- ^ The artist's staking key hash.
    }
PlutusTx.unstableMakeIsData ''CustomDatumType
-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtFractionalPid a == cdtFractionalPid b ) &&
           ( cdtTokenizedPid  a == cdtTokenizedPid  b ) &&
           ( cdtTokenizedTn   a == cdtTokenizedTn   b ) &&
           ( cdtArtistPKH     a == cdtArtistPKH     b ) &&
           ( cdtArtistSC      a == cdtArtistSC      b )
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Lock   |
                          Unlock |
                          Exit
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Lock,   0)
                                                , ('Unlock, 1)
                                                , ('Exit,   2)
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Lock -> do 
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh
      ; let b = traceIfFalse "Single Script Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let c = traceIfFalse "FT Mint Error"       checkMintedAmount                                  -- frac pid with tkn name
      ; let d = traceIfFalse "Datum Error"         $ isEmbeddedDatumIncreasing contOutputs            -- value with correct datum
      ;         traceIfFalse "Lock Endpoint Error" $ all (==True) [a,b,c,d]
      }
    Unlock -> do 
      { let a = traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info getPkh
      ; let b = traceIfFalse "Single Script Error"   $ isNInputs txInputs 1 && isNOutputs contOutputs 0
      ; let c = traceIfFalse "NFT Payout Error"      $ isAddrGettingPaid txOutputs artistAddr validatingValue
      ; let d = traceIfFalse "FT Burn Error"         checkMintedAmount
      ;         traceIfFalse "Unlock Endpoint Error" $ all (==True) [a,b,c,d]
      }
    Exit -> do 
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh
      ;         traceIfFalse "Exit Endpoint Error" $ all (==True) [a]
      }
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    -- inputs / outputs
    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    -- artist info
    artistPKH :: PlutusV2.PubKeyHash
    artistPKH = cdtArtistPKH datum

    artistSC :: PlutusV2.PubKeyHash
    artistSC = cdtArtistSC datum

    artistAddr :: PlutusV2.Address
    artistAddr =  createAddress artistPKH artistSC

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    singularNFT :: PlutusV2.Value
    singularNFT = Value.singleton tokenizedPid (cdtTokenizedTn datum) (1 :: Integer)

    -- minting
    checkMintedAmount :: Bool
    checkMintedAmount = 
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> cs == cdtFractionalPid datum && tn == cdtTokenizedTn datum
        _             -> traceIfFalse "Wrong pid and tkn name" False
    
    -- datum stuff
    isEmbeddedDatumIncreasing :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumIncreasing []     = False
    isEmbeddedDatumIncreasing (x:xs) =
      if PlutusV2.txOutValue x == (validatingValue + singularNFT)-- strict value continue
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
