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
-- | The starter token information.
-------------------------------------------------------------------------------
-- -- CurrencySymbol for the starter token.
-- lockPid :: PlutusV2.CurrencySymbol
-- lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [38, 144, 61, 231, 221, 148, 253, 203, 89, 253, 43, 89, 128, 168, 202, 79, 247, 31, 6, 47, 126, 210, 88, 89, 203, 38, 232, 127] }
-- -- TokenName for the starter token.
-- lockTkn :: PlutusV2.TokenName
-- lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = UsefulFuncs.createBuiltinByteString [78, 69, 87, 77, 95] }
-- -- Value for the starter token.
-- lockValue :: PlutusV2.Value
-- lockValue = Value.singleton lockPid lockTkn (1 :: Integer)
-- -------------------------------------------------------------------------------
-- -- | The validator hash of the LockStarterNFTContract.
-- -------------------------------------------------------------------------------
-- getValidatorHash :: PlutusV2.ValidatorHash
-- getValidatorHash = PlutusV2.ValidatorHash $ UsefulFuncs.createBuiltinByteString [29, 255, 27, 198, 146, 28, 252, 179, 246, 35, 187, 132, 173, 113, 140, 119, 233, 221, 170, 84, 213, 249, 5, 57, 12, 243, 217, 231]
-- -------------------------------------------------------------------------------
-- -- | The main public key hash for NEWM.
-- -------------------------------------------------------------------------------
-- getPkh :: PlutusV2.PubKeyHash
-- getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }
-- -------------------------------------------------------------------------------
-- -- | Hardcoded multisig participants.
-- -------------------------------------------------------------------------------
-- -- wallet 1
-- multiPkh1 :: PlutusV2.PubKeyHash
-- multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [150, 147, 223, 102, 202, 166, 174, 17, 93, 95, 24, 126, 236, 103, 146, 36, 158, 100, 86, 102, 7, 76, 76, 77, 115, 247, 147, 132] }
-- -- wallet 2
-- multiPkh2 :: PlutusV2.PubKeyHash
-- multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [213, 247, 65, 6, 15, 203, 170, 116, 238, 77, 158, 69, 116, 252, 176, 72, 211, 197, 56, 78, 192, 206, 73, 158, 8, 137, 190, 83] }
-- -- wallet 3
-- multiPkh3 :: PlutusV2.PubKeyHash
-- multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [67, 158, 82, 1, 141, 168, 20, 19, 240, 146, 132, 217, 97, 51, 160, 89, 193, 4, 222, 70, 42, 11, 29, 37, 211, 114, 106, 151] }
-- -------------------------------------------------------------------------------
-- -- | All possible signers inside the multisig
-- -------------------------------------------------------------------------------
-- listOfPkh :: [PlutusV2.PubKeyHash]
-- listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
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

-- plutusScript :: ScriptParameters -> Scripts.Script
-- plutusScript = PlutusV2.unMintingPolicyScript policy

-- validator :: ScriptParameters -> PlutusV2.Validator
-- validator = PlutusV2.Validator plutusScript

-- scriptAsCbor :: ScriptParameters -> LBS.ByteString
-- scriptAsCbor = serialise $ Plutonomy.optimizeUPLC $ validator
-- scriptAsCbor = serialise $ Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ validator

mintingPlutusScript :: ScriptParameters -> PlutusScript PlutusScriptV2
mintingPlutusScript sp = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict $ serialise $ Plutonomy.optimizeUPLC $ PlutusV2.Validator $ PlutusV2.unMintingPolicyScript (policy sp)