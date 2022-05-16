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

module MintingContract
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Ledger                   hiding (singleton)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Plutus.V1.Ledger.Value   as Value
import qualified Plutus.V1.Ledger.Address as Address
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           Codec.Serialise
-- import Data.Maybe
import           Data.Aeson                ( FromJSON, ToJSON )
import           Data.OpenApi.Schema       ( ToSchema )
import           GHC.Generics              ( Generic )
import           Prelude                   ( Show )
{-
  Author: Quinn Parkinson
  Rev: 0
-}
newtype MintParams = MintParams
  { mpValidatorHash :: ValidatorHash }
PlutusTx.makeLift ''MintParams

data CustomRedeemerType = CustomRedeemerType
    { cdtNewmPKH   :: !PubKeyHash
    , cdtNewmPid   :: !CurrencySymbol
    , cdtArtistPid :: !CurrencySymbol
    , cdtArtistTn  :: !TokenName
    , cdtArtistPKH :: !PubKeyHash
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp _ context = checkMintedAmount && checkSigner
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    -- redeemer' :: CustomRedeemerType
    -- redeemer' = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    isEmbeddedDatum :: [TxOut] -> Maybe CustomRedeemerType
    isEmbeddedDatum []     = Nothing
    isEmbeddedDatum (x:xs) =
      if txOutAddress x == Address.scriptHashAddress (mpValidatorHash mp)
      then
        case txOutDatumHash x of
          Nothing -> isEmbeddedDatum xs
          Just dh ->
            case findDatum dh info of
              Nothing        -> isEmbeddedDatum xs
              Just (Datum d) -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType d
      else isEmbeddedDatum xs

    checkSigner :: Bool
    -- checkSigner = traceIfFalse "Sign Error" $ txSignedBy info (cdtNewmPKH redeemer')
    checkSigner = traceIfFalse "Sign Error" signerFromTxOut
      where
        signerFromTxOut :: Bool
        signerFromTxOut =
          case isEmbeddedDatum txOutputs of
            Nothing -> traceIfFalse "Incorrect Datum" False
            Just datum' -> traceIfFalse "Incorrect Signer"  $ txSignedBy info (cdtNewmPKH datum')

    checkPolicyId :: CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ownCurrencySymbol context

    checkAmount :: Integer -> Bool
    checkAmount amt = traceIfFalse "Incorrect Mint/Burn Amount" $ amt == (100 :: Integer) || amt == (-100 :: Integer)
    -- checkAmount amt = traceIfFalse "Incorrect Mint/Burn Amount" $ amt == (100_000_000 :: Integer) || amt == (-100_000_000 :: Integer)

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && checkAmount amt
        _              -> traceIfFalse "Mint/Burn Error" False

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript ($$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript (policy $ MintParams { mpValidatorHash = "4618bf7d25472ff02e7598658022fad0be333f23183093267c6a7561"})

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