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
{-# LANGUAGE RecordWildCards       #-}
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
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley       ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
-- import qualified Plutus.V1.Ledger.Scripts  as Scripts
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Address  as Addr
import qualified Plutus.V2.Ledger.Contexts as ContextsV2
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified Plutonomy
-- importing only required functions for better readability
import qualified UsefulFuncs ( integerAsByteString
                             , checkValidMultisig
                             )
{-
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { starterPid :: PlutusV2.CurrencySymbol
  -- ^ Policy ID of the starter token
  , starterTkn :: PlutusV2.TokenName
  -- ^ Token name of the starter token\
  , validatorHash :: PlutusV2.ValidatorHash
  -- ^ The LockStarterNFTContract validator hash.
  , mainPkh    :: PlutusV2.PubKeyHash
  -- ^ The main public key hash for NEWM.
  , multiPkhs  :: [PlutusV2.PubKeyHash]
  -- ^ The multsig public key hashes for NEWM.
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create a token name using a prefix and an integer counter, i.e. token1, token2, etc.
-------------------------------------------------------------------------------
nftName :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
nftName prefix num = prefix <> UsefulFuncs.integerAsByteString num
-------------------------------------------------------------------------------
-- | Create the redeemer data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtNewmPid :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , cdtNumber  :: Integer
  -- ^ The starting number for the catalog.
  , cdtPrefix  :: PlutusV2.BuiltinByteString
  -- ^ The prefix for a catalog.
  }
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]

-- a is the old datum and b is the new datum
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtNewmPid a == cdtNewmPid b ) &&
           ( cdtNumber  a == cdtNumber  b ) &&
           ( cdtPrefix  a == cdtPrefix  b )
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptParameters -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy ScriptParameters {..} redeemer' context =  (traceIfFalse "Minting/Burning Error" $ (checkTokenMint redeemer) || (checkTokenBurn))                                         -- mint or burn
                           && (traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info mainPkh || UsefulFuncs.checkValidMultisig info multiPkhs 2)  -- newm or multisig
                           && (traceIfFalse "Invalid Datum Error"   $ checkInputDatum redeemer validatorHash)                                             -- input datum equals redeemer
                           && (traceIfFalse "Invalid Starter Token" $ Value.geq valueAtValidator starterValue)                                                  -- must contain the starter token
  where
    -- Value for the starter token.
    starterValue :: PlutusV2.Value
    starterValue = Value.singleton starterPid starterTkn (1 :: Integer)

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- | Convert the BuiltinData redeemer' into the CustomDatumType redeemer from the LockStarterNFTContract.
    redeemer :: CustomDatumType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomDatumType redeemer'
    
    -- | Return the inline datum from a tx out.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomDatumType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing       -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing       -- embedded datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->  -- inline datum
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing                    -- Bad Data
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        

    -- | Return the first inline datum from the LockStarterNFTContract from the list of inputs.
    checkInputs :: [PlutusV2.TxInInfo] -> PlutusV2.ValidatorHash -> Maybe CustomDatumType
    checkInputs []     _     = Nothing
    checkInputs (x:xs) vHash =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress vHash
        then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
        else checkInputs xs vHash

    -- | Check that the datum on the input from the LockStarterNFTContract is equal to the datum passed into the redeemer.
    checkInputDatum :: CustomDatumType -> PlutusV2.ValidatorHash -> Bool
    checkInputDatum customRedeemer vHash =
      case checkInputs txInputs vHash of
        Nothing         -> traceError "No Input Datum Hash"
        Just inputDatum -> inputDatum == customRedeemer
    
    -- | Get the value on the output going back to the LockStarterNFTContract.
    valueAtValidator :: PlutusV2.Value
    valueAtValidator = snd $ head $ ContextsV2.scriptOutputsAt validatorHash info

    -- | Check if exactly 1 token is being minted with a specific token name from the datum passed as the redeemer.
    checkTokenMint :: CustomDatumType -> Bool
    checkTokenMint customRedeemer =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs                  -- Must be this currency symbol
                         && checkTokenName customRedeemer tkn -- Must be the correct token name
                         && checkMintAmount amt               -- Must be a single token
        _                -> traceError "Mint/Burn Error"
    
    -- | Check if exactly 1 token is being burned. Token name here is irrelevant. 
    checkTokenBurn :: Bool
    checkTokenBurn =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs               -- Must be this currency symbol
                       && amt == (-1 :: Integer)         -- Must be a single token
        _                -> traceError "Mint/Burn Error"
    
    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkTokenName :: CustomDatumType -> PlutusV2.TokenName -> Bool
    checkTokenName customRedeemer tkn = traceIfFalse debug $ Value.unTokenName tkn == nftName (cdtPrefix customRedeemer) (cdtNumber customRedeemer)
      where
        -- This debugs the required token name.
        debug :: BuiltinString
        debug = decodeUtf8 $ "Required Token Name: " <>  nftName (cdtPrefix customRedeemer) (cdtNumber customRedeemer)

    checkMintAmount :: Integer -> Bool
    checkMintAmount amt = traceIfFalse "Incorrect Mint Amount" $ amt == (1 :: Integer)
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedPolicy :: ScriptParameters -> BuiltinData -> BuiltinData -> ()
wrappedPolicy s x y = check (mkPolicy s (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y))

policy :: ScriptParameters -> PlutusV2.MintingPolicy
policy sp = PlutusV2.mkMintingPolicyScript $ 
  $$(PlutusTx.compile [|| wrappedPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

mintingPlutusScript :: ScriptParameters -> PlutusScript PlutusScriptV2
mintingPlutusScript sp = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict $ serialise $ Plutonomy.optimizeUPLC $ PlutusV2.Validator $ PlutusV2.unMintingPolicyScript (policy sp)