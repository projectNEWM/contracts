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
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module V2MintingContract
  ( mintingPlutusScript
  , mintingScriptShortBs
  , getValidatorHash
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

{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
flattenBuiltinByteString [] = emptyByteString 
flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)

{-# INLINABLE getPkh #-}
getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = flattenBuiltinByteString [ consByteString x emptyByteString |x <- [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57]]}


getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ flattenBuiltinByteString [ consByteString x emptyByteString |x <- [54, 115, 246, 120, 231, 231, 81, 103, 156, 81, 112, 43, 213, 120, 240, 10, 170, 240, 99, 250, 173, 62, 47, 123, 11, 251, 99, 217]]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
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
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy _ context = checkMintedAmount && checkSigner
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    -- check if the incoming datum is the correct form.
    isEmbeddedDatum :: PlutusV2.TxOut -> Maybe CustomRedeemerType
    isEmbeddedDatum x = 
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> Nothing
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh)             -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> Nothing
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> Nothing
                Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType embedded
  

    checkSigner :: Bool
    checkSigner = traceIfFalse "Signer Error" $ signerFromTxOut txOutputs
      where
        signerFromTxOut :: [PlutusV2.TxOut] -> Bool
        signerFromTxOut []     = False
        signerFromTxOut (x:xs) =
          case isEmbeddedDatum x of
            Nothing     -> signerFromTxOut xs
            Just datum' -> 
              if PlutusV2.txOutAddress x == Addr.scriptHashAddress getValidatorHash --(cdtValidatorHash datum')
                then traceIfFalse "Incorrect Signer"  $ ContextsV2.txSignedBy info getPkh && ContextsV2.txSignedBy info (cdtArtistPKH datum')
                else signerFromTxOut xs

    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkAmount :: Integer -> Bool
    checkAmount amt = traceIfFalse "Incorrect Mint/Burn Amount" $ amt == (100_000_000 :: Integer) || amt == (-100_000_000 :: Integer)

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && checkAmount amt
        _              -> traceIfFalse "Mint/Burn Error" False
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
policy :: PlutusV2.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Utils.mkUntypedMintingPolicy mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor