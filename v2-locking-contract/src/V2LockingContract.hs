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
import           V2CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
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
    , cdtNewmPKH       :: PlutusV2.PubKeyHash
    -- ^ The newm's public key hash.
    }
PlutusTx.unstableMakeIsData ''CustomDatumType
-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtFractionalPid a == cdtFractionalPid b ) &&
           ( cdtTokenizedPid  a == cdtTokenizedPid  b ) &&
           ( cdtTokenizedTn   a == cdtTokenizedTn   b ) &&
           ( cdtArtistPKH     a == cdtArtistPKH     b ) &&
           ( cdtArtistSC      a == cdtArtistSC      b ) &&
           ( cdtNewmPKH       a == cdtNewmPKH       b )
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
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info newmPKH && ContextsV2.txSignedBy info artistPKH
      ; let b = traceIfFalse "Single Script Error" $ isSingleScript txInputs
      ; let c = traceIfFalse "Cont Payin Error"    $ isValueContinuing contOutputs (validatingValue + singularNFT)
      ; let d = traceIfFalse "FT Mint Error"       checkMintedAmount
      ; let e = traceIfFalse "Datum Error"         $ isEmbeddedDatum contOutputs
      ;         traceIfFalse "Lock Endpoint Error" $ all (==True) [a,b,c,d,e]
      }
    Unlock -> do 
      { let a = traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info newmPKH && ContextsV2.txSignedBy info artistPKH
      ; let b = traceIfFalse "Single Script Error"   $ isSingleScript txInputs
      ; let c = traceIfFalse "NFT Payout Error"      $ isAddrGettingPaid txOutputs artistAddr singularNFT
      ; let d = traceIfFalse "Cont Payin Error"      $ isValueContinuing contOutputs (validatingValue - singularNFT)
      ; let e = traceIfFalse "FT Burn Error"         checkMintedAmount
      ; let f = traceIfFalse "Datum Error"           $ isEmbeddedDatum contOutputs
      ;         traceIfFalse "Unlock Endpoint Error" $ all (==True) [a,b,c,d,e,f]
      }
    Exit -> do 
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info newmPKH
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

    -- newm info
    newmPKH :: PlutusV2.PubKeyHash
    newmPKH = cdtNewmPKH datum

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
    singularNFT = Value.singleton (cdtTokenizedPid datum) (cdtTokenizedTn datum) (1 :: Integer)

    -- minting
    checkMintedAmount :: Bool
    checkMintedAmount = 
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> cs == cdtFractionalPid datum && tn == cdtTokenizedTn datum
        _             -> False
    
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
