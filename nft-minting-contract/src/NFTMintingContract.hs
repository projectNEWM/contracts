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
module NFTMintingContract
  ( mintingPlutusScript
  , mintingScriptShortBs
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
import           TokenHelper
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [152, 47, 147, 160, 239, 222, 142, 221, 14, 154, 244, 0, 218, 8, 62, 145, 217, 142, 29, 91, 74, 119, 160, 121, 56, 164, 222, 79] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [116, 104, 105, 115, 105, 115, 97, 118, 101, 114, 121, 108, 111, 110, 103, 115, 116, 114, 105, 110, 103, 102, 111, 114, 116, 101, 115, 116, 105, 110, 49, 48] }

-- check for nft here
tokenValue :: PlutusV2.Value
tokenValue = Value.singleton lockPid lockTkn (1 :: Integer)

{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
flattenBuiltinByteString [] = emptyByteString 
flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)

{-# INLINABLE createBuiltinByteString #-}
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString |x <- intList]

getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [40, 17, 145, 5, 237, 60, 42, 35, 203, 80, 206, 249, 32, 89, 207, 207, 9, 37, 110, 151, 196, 51, 186, 218, 52, 106, 140, 136]

data CustomRedeemerType = CustomRedeemerType
  { crtNewmPid :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , crtNumber  :: Integer
  -- ^ The starting number for the catalog.
  , crtPrefix  :: PlutusV2.BuiltinByteString
  -- ^ The prefix for a catalog.
  }
PlutusTx.unstableMakeIsData ''CustomRedeemerType

-- old == new | minting
instance Eq CustomRedeemerType where
  {-# INLINABLE (==) #-}
  a == b = ( crtNewmPid a == crtNewmPid b ) &&
           ( crtNumber  a == crtNumber  b ) &&
           ( crtPrefix  a == crtPrefix  b )
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer' context = do
      { let a = traceIfFalse "Minting/Burning Error" $ (checkTokenMint && checkOutputDatum 1) || (checkTokenBurn && checkOutputDatum 0) -- mint or burn check
      ; let b = traceIfFalse "Incorrect Signer"      $ ContextsV2.txSignedBy info getPkh          -- newm signs it
      ; let c = traceIfFalse "Input Datum Error"     checkInputDatum                             -- the input datum is equal what is being pass into the redeemer
      ; let d = traceIfFalse "Incorrect Start Token" $ Value.geq valueAtValidator tokenValue -- must contain the starter token
      ;         traceIfFalse "Minting Contract Endpoint Error" $ all (==True) [a,b,c,d]
      }
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- the redeemer is the datum of the locking script
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'
    
    -- check if the incoming datum is the correct form.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomRedeemerType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> Nothing
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> Nothing
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> Nothing
                Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType embedded

    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> Maybe CustomRedeemerType
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress getValidatorHash
      then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
      else checkInputs xs

    -- check that the locking script has the correct datum hash
    checkInputDatum :: Bool
    checkInputDatum =
      case checkInputs txInputs of
        Nothing -> traceIfFalse "No Input Datum Hash" False
        Just inputDatum -> inputDatum == d
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer
              , crtNumber  = crtNumber  redeemer
              , crtPrefix  = crtPrefix  redeemer
              }
    
    -- find the value at the validator hash
    valueAtValidator :: PlutusV2.Value
    valueAtValidator = snd $ head $ ContextsV2.scriptOutputsAt getValidatorHash info

    datumAtValidator :: Maybe CustomRedeemerType
    datumAtValidator = 
      case datumAtValidator' of
        -- datumless
        PlutusV2.NoOutputDatum -> Nothing
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> Nothing
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> Nothing
                Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType embedded
      where datumAtValidator' = fst $ head $ ContextsV2.scriptOutputsAt getValidatorHash info

    -- the output datum for minting increases the number by one
    checkOutputDatum :: Integer -> Bool
    checkOutputDatum increment = 
      case datumAtValidator of
        Nothing -> traceIfFalse "No Datum At Validator" False
        Just datum'' -> datum'' == d
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer
              , crtNumber  = crtNumber  redeemer + increment
              , crtPrefix  = crtPrefix  redeemer
              }

    -- check the minting stuff here
    checkTokenMint :: Bool
    checkTokenMint =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs && checkTokenName tkn && checkMintAmount amt
        _                -> traceIfFalse "Mint/Burn Error" False
    
    -- check the burning stuff here
    checkTokenBurn :: Bool
    checkTokenBurn =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && amt == (-1 :: Integer)
        _                -> traceIfFalse "Mint/Burn Error" False
    
    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkTokenName :: PlutusV2.TokenName -> Bool
    checkTokenName tkn = traceIfFalse debug $ Value.unTokenName tkn == nftName (crtPrefix redeemer) (crtNumber redeemer)
      where
        -- it debugs the required NFT name
        debug :: BuiltinString
        debug = decodeUtf8 $ "Required Token Name: " <>  nftName (crtPrefix redeemer) (crtNumber redeemer)

    checkMintAmount :: Integer -> Bool
    checkMintAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (1 :: Integer)

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