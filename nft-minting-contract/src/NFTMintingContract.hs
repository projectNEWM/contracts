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
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Ledger                   hiding (singleton)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Plutus.V1.Ledger.Value   as Value
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Address as Address
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           Codec.Serialise
import           Data.Aeson                ( FromJSON, ToJSON )
import           Data.OpenApi.Schema       ( ToSchema )
import           GHC.Generics              ( Generic )
import           Prelude                   ( Show )
import TokenHelper
{-
  Author: Quinn Parkinson
  Rev: 0
-}
data MintParams = MintParams
  { mpValidatorHash :: !ValidatorHash
  , mpNewmPKH       :: !PubKeyHash
  }
PlutusTx.makeLift ''MintParams

data CustomRedeemerType = CustomRedeemerType
    { crtNewmPid :: !CurrencySymbol
    , crtNumber  :: !Integer
    , crtPrefix  :: !BuiltinByteString
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp redeemer context = checkMintedAmount && checkSigner && checkVal && checkInputDatum && checkOutputDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    checkInputs :: [TxInInfo] -> Maybe DatumHash
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if txOutAddress (txInInfoResolved x) == Address.scriptHashAddress (mpValidatorHash mp)
      then txOutDatumHash (txInInfoResolved x)
      else checkInputs xs

    checkInputDatum :: Bool
    checkInputDatum =
      case checkInputs txInputs of
        Nothing -> traceIfFalse "No Input Datum Hash" False
        Just dh ->
          case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
            Nothing -> traceIfFalse "No Check Datum Hash" False
            Just dh' -> dh == dh'
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer'
              , crtNumber = crtNumber redeemer'
              , crtPrefix = crtPrefix redeemer'
              }
    
    valueAtValidator :: Value
    valueAtValidator = snd $ head $ scriptOutputsAt (mpValidatorHash mp) info

    checkVal :: Bool
    checkVal = traceIfFalse "Incorrect Script Amount" $ Ada.lovelaceValueOf (5_000_000 :: Integer) == valueAtValidator

    datumHashAtValidator :: DatumHash
    datumHashAtValidator = fst $ head $ scriptOutputsAt (mpValidatorHash mp) info

    checkOutputDatum =
      case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
        Nothing -> traceIfFalse "No Datum Hash" False
        Just dh -> dh == datumHashAtValidator
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer'
              , crtNumber = crtNumber redeemer'+1
              , crtPrefix = crtPrefix redeemer'
              }

    redeemer' :: CustomRedeemerType
    redeemer' = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer

    checkSigner :: Bool
    checkSigner = traceIfFalse "Incorrect Signer" $ txSignedBy info (mpNewmPKH mp)

    checkPolicyId :: CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ownCurrencySymbol context

    checkAmount :: Integer -> Bool
    checkAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (1 :: Integer)

    checkTkn :: TokenName -> Bool
    checkTkn tkn = traceIfFalse debug $ Value.unTokenName tkn == nftName (crtPrefix redeemer') (crtNumber redeemer')
      where
        debug :: BuiltinString
        debug = decodeUtf8 $ nftName (crtPrefix redeemer') (crtNumber redeemer')

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs && checkTkn tkn && checkAmount amt
        _                -> traceIfFalse "Mint/Burn Error" False

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript ($$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript $ policy params
  where
    params = MintParams { mpValidatorHash = "2ce17650d458c78ed350adaf0bea254b38c7591d04e6ea7c890ebc03"
                        , mpNewmPKH       = "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439"
                        }

validator :: Validator
validator = Validator plutusScript

-------------------------------------------------------------------------------
-- Do Not Remove
scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor