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
module V2LockingContract
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
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           V2CheckFuncs
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
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }

voteValidatorHash :: PlutusV2.ValidatorHash
voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [45, 120, 123, 140, 75, 245, 182, 82, 209, 137, 32, 129, 200, 229, 84, 212, 13, 59, 140, 156, 7, 94, 119, 178, 248, 236, 19, 215]

-- tokenization minting policy
tokenizedPid :: PlutusV2.CurrencySymbol
tokenizedPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [196, 87, 32, 34, 188, 247, 68, 86, 247, 119, 67, 83, 244, 37, 126, 202, 193, 162, 44, 146, 198, 162, 243, 186, 55, 39, 147, 60] }

-- voting starter nft
voteStartPid :: PlutusV2.CurrencySymbol
voteStartPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString [152, 47, 147, 160, 239, 222, 142, 221, 14, 154, 244, 0, 218, 8, 62, 145, 217, 142, 29, 91, 74, 119, 160, 121, 56, 164, 222, 79] }

voteStartTkn :: PlutusV2.TokenName
voteStartTkn = PlutusV2.TokenName { PlutusV2.unTokenName = createBuiltinByteString [116, 104, 105, 115, 105, 115, 97, 118, 101, 114, 121, 108, 111, 110, 103, 115, 116, 114, 105, 110, 103, 102, 111, 114, 116, 101, 115, 116, 105, 110, 49, 48] }

voteStartValue :: PlutusV2.Value
voteStartValue = Value.singleton voteStartPid voteStartTkn (1 :: Integer)
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
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtFractionalPid :: PlutusV2.CurrencySymbol
    -- ^ The Newm fractionalization minting policy
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
      { let a = traceIfFalse "Vote Has Failed"     $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error" $ isSingleScript txInputs && isSingleScript txRefInputs
      ; let c = traceIfFalse "Cont Payin Error"    $ isValueContinuing contOutputs (validatingValue + tokenizedNFT)
      ; let d = traceIfFalse "FT Mint Error"       checkMintingProcess
      ; let e = traceIfFalse "Datum Error"         $ isEmbeddedDatum contOutputs
      ;         traceIfFalse "Lock Endpoint Error" $ all (==True) [a,b,c,d,e]
      }
    Unlock -> do 
      { let a = traceIfFalse "Vote Has Failed"       $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error"   $ isSingleScript txInputs && isSingleScript txRefInputs
      ; let c = traceIfFalse "NFT Payout Error"      $ isAddrGettingPaid txOutputs artistAddr validatingValue
      ; let d = traceIfFalse "FT Burn Error"         checkMintingProcess
      ;         traceIfFalse "Unlock Endpoint Error" $ all (==True) [a,b,c,d]
      }
    Exit -> do -- remove in production
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

    txRefInputs :: [PlutusV2.TxInInfo]
    txRefInputs = PlutusV2.txInfoReferenceInputs info

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
    
    tokenizedNFT :: PlutusV2.Value
    tokenizedNFT = Value.singleton tokenizedPid (cdtTokenizedTn datum) (1 :: Integer)

    -- minting
    checkMintingProcess :: Bool
    checkMintingProcess = 
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> cs == cdtFractionalPid datum && tn == cdtTokenizedTn datum
        _             -> False
    
    -- check for vote nft here
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
    
    -- voteStartValue :: PlutusV2.Value
    -- voteStartValue = Value.singleton votePid voteTkn (1 :: Integer)

    checkVoteFromDatum :: [PlutusV2.TxInInfo] -> Bool
    checkVoteFromDatum []     = traceIfFalse "No Datum Found on Reference Input" False
    checkVoteFromDatum (x:xs) =
      if traceIfFalse "Incorrect Validator Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress voteValidatorHash
        then
          if traceIfFalse "Incorrect Starter Value" $ Value.geq (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) voteStartValue
            then
              case getReferenceDatum $ PlutusV2.txInInfoResolved x of
                Nothing -> checkVoteFromDatum xs
                Just voteDatum -> isVoteComplete (vdtPid voteDatum) (vdtTkn voteDatum) (vdtAmt voteDatum) info
            else checkVoteFromDatum xs
        else checkVoteFromDatum xs
    

    -- check if the incoming datum is the correct form.
    isEmbeddedDatum :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatum []     = False
    isEmbeddedDatum (x:xs) = 
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> isEmbeddedDatum xs
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> isEmbeddedDatum xs
            Just inline -> datum == inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh)             -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> isEmbeddedDatum xs
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> isEmbeddedDatum xs
                Just embedded -> datum == embedded
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
