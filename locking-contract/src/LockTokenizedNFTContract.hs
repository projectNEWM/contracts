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
-------------------------------------------------------------------------------
-- | The main public key hash for NEWM.
-------------------------------------------------------------------------------
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }
-------------------------------------------------------------------------------
-- | The validator hash of the LockStarterNFTContract.
-------------------------------------------------------------------------------
tokenizedPid :: PlutusV2.CurrencySymbol
tokenizedPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [61, 115, 175, 75, 186, 42, 202, 107, 76, 2, 123, 190, 173, 45, 29, 99, 50, 56, 76, 32, 255, 197, 104, 230, 246, 241, 96, 189] }
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

-- a is old datum and b is the new datum
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
    Lock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh)                        -- newm signs it
         && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                         -- single script input
         && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                     -- single script output
         && (traceIfFalse "NFT Minting Error"   checkMintedAmount)                                          -- mint an nft only
         && (traceIfFalse "Invalid Datum Error" $ isDatumConstant contOutputs validatingValue singularNFT)  -- value is cont and the datum is correct.

    -- | Unlock Tokenized Token from contract and solidify.
    Unlock -> (traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh)                      -- newm signs it
           && (traceIfFalse "Single Input Error"  $ UsefulFuncs.isNInputs txInputs 1)                       -- single script input
           && (traceIfFalse "Single Output Error" $ UsefulFuncs.isNOutputs contOutputs 1)                   -- single script output
           && (traceIfFalse "NFT Payout Error"    $ UsefulFuncs.isAddrGettingPaidExactly txOutputs artistAddr validatingValue) -- artist get everything back
           && (traceIfFalse "NFT Minting Error"   checkMintedAmount)                                        -- mint an nft only
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

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

    -- | This is the currently validating value from the UTxO being spent in this tx.
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate"
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- | This is the NFT coming into the contract to be fractionalized.
    singularNFT :: PlutusV2.Value
    singularNFT = Value.singleton tokenizedPid (cdtTokenizedTn datum) (1 :: Integer)

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
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))

validator :: PlutusV2.Validator
validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
-- validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs