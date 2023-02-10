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
module LockTokenizedNFTContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley                             ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                                 ( serialise )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V1.Ledger.Scripts                        as Scripts
import qualified Plutus.V1.Ledger.Value                          as Value
import qualified Plutus.V2.Ledger.Contexts                       as ContextsV2
import qualified Plutus.V2.Ledger.Api                            as PlutusV2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
-- importing only required functions for better readability
import qualified UsefulFuncs ( createBuiltinByteString
                             , isNInputs
                             , isNOutputs
                             , isAddrGettingPaidExactly
                             , createAddress
                             )
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}

mainPkh :: PlutusV2.PubKeyHash
mainPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

-- tokenization minting policy
tokenizedPid :: PlutusV2.CurrencySymbol
tokenizedPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [193, 248, 13, 144, 80, 184, 99, 46, 243, 114, 221, 102, 180, 194, 131, 15, 156, 123, 195, 249, 176, 248, 20, 166, 84, 150, 64, 39] }

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
                          Unlock
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Lock,   0 )
                                                , ( 'Unlock, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    -- | Lock Tokenized Token into contract and fractionalized
    Lock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info mainPkh)                     -- newm signs it
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                       -- single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                   -- single script output
         && (traceIfFalse "NFT Minting Error"   checkMintedAmount)                                        -- mint an nft only
         && (traceIfFalse "Invalid Datum Error" $ isEmbeddedDatumConstant contOutputs validatingValue singularNFT)  -- value is cont and the datum is correct.

    -- | Unlock Tokenized Token from contract and solidify.
    Unlock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info mainPkh)                     -- newm signs it
           && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                       -- single script input
           && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                   -- single script output
           && (traceIfFalse "NFT Payout Error"    $ UsefulFuncs.isAddrGettingPaidExactly txOutputs artistAddr validatingValue) -- artist get everything back
           && (traceIfFalse "NFT Minting Error"   checkMintedAmount)                                        -- mint an nft only
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
    artistAddr =  UsefulFuncs.createAddress artistPKH artistSC

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- what is being sent into the fractionalize contract upon locking
    singularNFT :: PlutusV2.Value
    singularNFT = Value.singleton tokenizedPid (cdtTokenizedTn datum) (1 :: Integer)

    -- minting amount is handled in the minting contract
    checkMintedAmount :: Bool
    checkMintedAmount = 
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tn, _)] -> cs == cdtFractionalPid datum
                      && tn == cdtTokenizedTn datum
        _             -> traceIfFalse "Wrong pid and tkn name" False
    
    -- datum stuff
    isEmbeddedDatumConstant :: [PlutusV2.TxOut] -> PlutusV2.Value -> PlutusV2.Value -> Bool
    isEmbeddedDatumConstant []     _   _   = traceError "No Constant Datum Found"
    isEmbeddedDatumConstant (x:xs) val nft =
      if PlutusV2.txOutValue x == (val + nft) -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> traceError "No Datum" -- datumless
            (PlutusV2.OutputDatumHash _) -> traceError "Embedded" -- embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     ->  traceError "Bad Datum"
                Just inline -> datum == inline
        else isEmbeddedDatumConstant xs val nft
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
