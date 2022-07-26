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
import           Codec.Serialise
import qualified PlutusTx
import           Ledger                   hiding (singleton)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           Plutus.V1.Ledger.Value   as Value
import qualified Plutus.V1.Ledger.Address as Address
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Ledger.Typed.Scripts     as Scripts
import           Cardano.Api.Shelley      ( PlutusScript (..), PlutusScriptV1 )
import           Data.Aeson               ( FromJSON, ToJSON )
import           Data.OpenApi.Schema      ( ToSchema )
import           GHC.Generics             ( Generic )
import           Prelude                  ( Show )
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
data MintParams = MintParams
  { mpValidatorHash :: !ValidatorHash
  -- ^ Validator hash of the locking script
  , mpNewmPKH       :: !PubKeyHash
  -- ^ Official Newm public key hash.
  }
PlutusTx.makeLift ''MintParams

data CustomRedeemerType = CustomRedeemerType
    { cdtNewmPid   :: !CurrencySymbol
    -- ^ The Newm fractionalization minting policy
    , cdtArtistPid :: !CurrencySymbol
    -- ^ The artist's tokenized policy id
    , cdtArtistTn  :: !TokenName
    -- ^ the artist's tokenized token name.
    , cdtArtistPKH :: !PubKeyHash
    -- ^ The artist's public key hash.
    , cdtArtistSC ::  !PubKeyHash
    -- ^ The artist's staking key hash.
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
    checkSigner = traceIfFalse "Signer Error" signerFromTxOut
      where
        signerFromTxOut :: Bool
        signerFromTxOut =
          case isEmbeddedDatum txOutputs of
            Nothing     -> traceIfFalse "No Datum Found" False
            Just datum' -> traceIfFalse "Incorrect Signer"  $ txSignedBy info (mpNewmPKH mp) && txSignedBy info (cdtArtistPKH datum')

    checkPolicyId :: CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ownCurrencySymbol context

    checkAmount :: Integer -> Bool
    checkAmount amt = traceIfFalse "Incorrect Mint/Burn Amount" $ amt == (100_000_000 :: Integer) || amt == (-100_000_000 :: Integer)

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
plutusScript = unMintingPolicyScript (policy params)
  where params = MintParams { mpValidatorHash = "49cf1d9cb7c80faac818e2e3673f06f381e2e4a9cd40e75e91e4f440" -- locking script
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