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
import           Cardano.Api.Shelley      ( PlutusScript (..), PlutusScriptV1 )
import           Plutus.V1.Ledger.Value   as Value
import qualified Plutus.V1.Ledger.Ada     as Ada
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
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
data MintParams = MintParams
  { mpValidatorHash :: !ValidatorHash
  -- ^ The locking script validator hash.
  , mpNewmPKH       :: !PubKeyHash
  -- ^ The Newm public key hash.
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
mkPolicy mp redeemer context = do
      { let a = traceIfFalse "Minting Error"      checkTokenMint && checkIncreasingOutputDatum || checkTokenBurn && checkConstantOutputDatum
      ; let b = traceIfFalse "Signing Error"      checkSigner
      ; let c = traceIfFalse "Min ADA Error"      checkVal
      ; let d = traceIfFalse "Input Datum Error"  checkInputDatum
      ;         traceIfFalse "Minting Endpoint Error" $ all (==True) [a,b,c,d]
      }
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    -- the redeemer is the datum of the locking script
    redeemer' :: CustomRedeemerType
    redeemer' = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer

    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [TxInInfo] -> Maybe DatumHash
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if txOutAddress (txInInfoResolved x) == Address.scriptHashAddress (mpValidatorHash mp)
      then txOutDatumHash (txInInfoResolved x)
      else checkInputs xs

    -- check that the locking script has the correct datum hash
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
              , crtNumber  = crtNumber  redeemer'
              , crtPrefix  = crtPrefix  redeemer'
              }
    
    valueAtValidator :: Value
    valueAtValidator = snd $ head $ scriptOutputsAt (mpValidatorHash mp) info

    -- a decentrlized approach to this would be looking for an nft and not an ada amount.
    checkVal :: Bool
    checkVal = traceIfFalse "Incorrect Script Amount" $ Ada.lovelaceValueOf (5_000_000 :: Integer) == valueAtValidator

    datumHashAtValidator :: DatumHash
    datumHashAtValidator = fst $ head $ scriptOutputsAt (mpValidatorHash mp) info

    checkIncreasingOutputDatum :: Bool
    checkIncreasingOutputDatum =
      case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
        Nothing -> traceIfFalse "No Datum Hash" False
        Just dh -> dh == datumHashAtValidator
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer'
              , crtNumber  = crtNumber  redeemer' + 1
              , crtPrefix  = crtPrefix  redeemer'
              }
    
    checkConstantOutputDatum :: Bool
    checkConstantOutputDatum =
      case findDatumHash (Datum $ PlutusTx.toBuiltinData d) info of
        Nothing -> traceIfFalse "No Datum Hash" False
        Just dh -> dh == datumHashAtValidator
      where
        d :: CustomRedeemerType
        d = CustomRedeemerType
              { crtNewmPid = crtNewmPid redeemer'
              , crtNumber  = crtNumber  redeemer'
              , crtPrefix  = crtPrefix  redeemer'
              }

    -- only newm can mint it
    checkSigner :: Bool
    checkSigner = traceIfFalse "Incorrect Signer" $ txSignedBy info (mpNewmPKH mp)

    -- check the minting stuff here
    checkTokenMint :: Bool
    checkTokenMint =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs && checkTokenName tkn && checkMintAmount amt
        _                -> traceIfFalse "Mint/Burn Error" False
    
    -- check the minting stuff here
    checkTokenBurn :: Bool
    checkTokenBurn =
      case Value.flattenValue (txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && amt == (-1 :: Integer)
        _                -> traceIfFalse "Mint/Burn Error" False
    
    checkPolicyId :: CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ownCurrencySymbol context

    checkTokenName :: TokenName -> Bool
    checkTokenName tkn = traceIfFalse debug $ Value.unTokenName tkn == nftName (crtPrefix redeemer') (crtNumber redeemer')
      where
        debug :: BuiltinString
        debug = decodeUtf8 $ nftName (crtPrefix redeemer') (crtNumber redeemer')

    checkMintAmount :: Integer -> Bool
    checkMintAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (1 :: Integer)

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript ($$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript $ policy params
  where
    params = MintParams { mpValidatorHash = "f4b38fbbad1937f227e3a59cae26fa3c9eaa6ca89f938f0d02b7d92d" -- locking script
                        , mpNewmPKH       = "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439" -- newm pkh
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