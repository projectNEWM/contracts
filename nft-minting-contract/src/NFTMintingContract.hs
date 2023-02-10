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
import           Cardano.Api.Shelley                                   ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                                       ( serialise )
import qualified Data.ByteString.Lazy                                  as LBS
import qualified Data.ByteString.Short                                 as SBS
import qualified Plutus.V1.Ledger.Scripts                              as Scripts
import qualified Plutus.V1.Ledger.Value                                as Value
import qualified Plutus.V1.Ledger.Address                              as Addr
import qualified Plutus.V2.Ledger.Contexts                             as ContextsV2
import qualified Plutus.V2.Ledger.Api                                  as PlutusV2
import           Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Utils
-- importing only required functions for better readability
import qualified UsefulFuncs ( createBuiltinByteString
                             , integerAsByteString
                             , checkValidMultisig
                             )
-- import LockStarterNFTContract (CustomDatumType (..))
{-
  Author   : The Ancient Kraken
  Copyright: 2023
  Version  : Rev 2
-}
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString [38, 144, 61, 231, 221, 148, 253, 203, 89, 253, 43, 89, 128, 168, 202, 79, 247, 31, 6, 47, 126, 210, 88, 89, 203, 38, 232, 127] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = UsefulFuncs.createBuiltinByteString [78, 69, 87, 77, 95] }

-- check for nft here
tokenValue :: PlutusV2.Value
tokenValue = Value.singleton lockPid lockTkn (1 :: Integer)

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ UsefulFuncs.createBuiltinByteString [105, 112, 91, 98, 149, 147, 245, 132, 156, 161, 50, 175, 41, 211, 252, 75, 86, 35, 163, 25, 32, 121, 94, 115, 88, 184, 121, 62]

getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

-- collat wallet
multiPkh1 :: PlutusV2.PubKeyHash
multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [150, 147, 223, 102, 202, 166, 174, 17, 93, 95, 24, 126, 236, 103, 146, 36, 158, 100, 86, 102, 7, 76, 76, 77, 115, 247, 147, 132] }
-- seller wallet
multiPkh2 :: PlutusV2.PubKeyHash
multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [213, 247, 65, 6, 15, 203, 170, 116, 238, 77, 158, 69, 116, 252, 176, 72, 211, 197, 56, 78, 192, 206, 73, 158, 8, 137, 190, 83] }
-- reference wallet
multiPkh3 :: PlutusV2.PubKeyHash
multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = UsefulFuncs.createBuiltinByteString [67, 158, 82, 1, 141, 168, 20, 19, 240, 146, 132, 217, 97, 51, 160, 89, 193, 4, 222, 70, 42, 11, 29, 37, 211, 114, 106, 151] }

-- all possible signers
listOfPkh :: [PlutusV2.PubKeyHash]
listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
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
PlutusTx.unstableMakeIsData ''CustomDatumType

-- old == new | minting
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtNewmPid a == cdtNewmPid b ) &&
           ( cdtNumber  a == cdtNumber  b ) &&
           ( cdtPrefix  a == cdtPrefix  b )
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer' context =  (traceIfFalse "Minting/Burning Error" $ (checkTokenMint redeemer && checkOutputDatum redeemer 1) || (checkTokenBurn && checkOutputDatum redeemer 0)) -- mint or burn
                           && (traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info getPkh || UsefulFuncs.checkValidMultisig info listOfPkh 2)         -- newm or multisig
                           && (traceIfFalse "Invalid Datum Error"   $ checkInputDatum redeemer getValidatorHash)                                                                    -- input datum equals redeemer
                           && (traceIfFalse "Invalid Starter Token" $ Value.geq valueAtValidator tokenValue)                                            -- must contain the starter token
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    -- the redeemer is the datum of the locking script
    redeemer :: CustomDatumType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomDatumType redeemer'
    
    -- check if the incoming datum is the correct form.
    getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomDatumType
    getDatumFromTxOut x = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        

    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> PlutusV2.ValidatorHash -> Maybe CustomDatumType
    checkInputs []     _     = Nothing
    checkInputs (x:xs) vHash =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress vHash
      then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
      else checkInputs xs vHash

    -- check that the locking script has the correct datum hash
    checkInputDatum :: CustomDatumType -> PlutusV2.ValidatorHash -> Bool
    checkInputDatum customRedeemer vHash =
      case checkInputs txInputs vHash of
        Nothing         -> traceError "No Input Datum Hash"
        Just inputDatum -> inputDatum == d
      where
        d :: CustomDatumType
        d = CustomDatumType
              { cdtNewmPid = cdtNewmPid customRedeemer
              , cdtNumber  = cdtNumber  customRedeemer
              , cdtPrefix  = cdtPrefix  customRedeemer
              }
    
    -- find the value at the validator hash
    valueAtValidator :: PlutusV2.Value
    valueAtValidator = snd $ head $ ContextsV2.scriptOutputsAt getValidatorHash info

    datumAtValidator :: Maybe CustomDatumType
    datumAtValidator =
      if length scriptOutputs == 0 
        then Nothing
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> Nothing -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
      where 
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt getValidatorHash info
        
    -- the output datum for minting increases the number by one
    checkOutputDatum :: CustomDatumType -> Integer -> Bool
    checkOutputDatum customRedeemer increment = 
      case datumAtValidator of
        Nothing      -> traceError "No Datum At Validator"
        Just datum'' -> datum'' == d
      where
        d :: CustomDatumType
        d = CustomDatumType
              { cdtNewmPid = cdtNewmPid customRedeemer
              , cdtNumber  = cdtNumber  customRedeemer + increment
              , cdtPrefix  = cdtPrefix  customRedeemer
              }

    -- check the minting stuff here
    checkTokenMint :: CustomDatumType -> Bool
    checkTokenMint customRedeemer =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> checkPolicyId cs && checkTokenName customRedeemer tkn && checkMintAmount amt
        _                -> traceIfFalse "Mint/Burn Error" False
    
    -- check the burning stuff here
    checkTokenBurn :: Bool
    checkTokenBurn =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, _, amt)] -> checkPolicyId cs && amt == (-1 :: Integer)
        _                -> traceIfFalse "Mint/Burn Error" False
    
    checkPolicyId :: PlutusV2.CurrencySymbol ->  Bool
    checkPolicyId cs = traceIfFalse "Incorrect Policy Id" $ cs == ContextsV2.ownCurrencySymbol context

    checkTokenName :: CustomDatumType -> PlutusV2.TokenName -> Bool
    checkTokenName customRedeemer tkn = traceIfFalse debug $ Value.unTokenName tkn == nftName (cdtPrefix customRedeemer) (cdtNumber customRedeemer)
      where
        -- it debugs the required NFT name
        debug :: BuiltinString
        debug = decodeUtf8 $ "Required Token Name: " <>  nftName (cdtPrefix customRedeemer) (cdtNumber customRedeemer)

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