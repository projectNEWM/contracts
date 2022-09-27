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
import           UsefulFuncs
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [38, 144, 61, 231, 221, 148, 253, 203, 89, 253, 43, 89, 128, 168, 202, 79, 247, 31, 6, 47, 126, 210, 88, 89, 203, 38, 232, 127] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [78, 69, 87, 77, 95] }

-- check for nft here
tokenValue :: PlutusV2.Value
tokenValue = Value.singleton lockPid lockTkn (1 :: Integer)

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [103, 232, 225, 18, 171, 5, 18, 201, 201, 115, 124, 209, 55, 70, 38, 122, 160, 53, 253, 115, 216, 169, 99, 20, 3, 144, 8, 6]

getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [124, 31, 212, 29, 225, 74, 57, 151, 130, 90, 250, 45, 84, 166, 94, 219, 125, 37, 60, 149, 200, 61, 64, 12, 99, 102, 222, 164] }

-- collat wallet
multiPkh1 :: PlutusV2.PubKeyHash
multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [150, 147, 223, 102, 202, 166, 174, 17, 93, 95, 24, 126, 236, 103, 146, 36, 158, 100, 86, 102, 7, 76, 76, 77, 115, 247, 147, 132] }
-- seller wallet
multiPkh2 :: PlutusV2.PubKeyHash
multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [213, 247, 65, 6, 15, 203, 170, 116, 238, 77, 158, 69, 116, 252, 176, 72, 211, 197, 56, 78, 192, 206, 73, 158, 8, 137, 190, 83] }
-- reference wallet
multiPkh3 :: PlutusV2.PubKeyHash
multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [67, 158, 82, 1, 141, 168, 20, 19, 240, 146, 132, 217, 97, 51, 160, 89, 193, 4, 222, 70, 42, 11, 29, 37, 211, 114, 106, 151] }

-- all possible signers
listOfPkh :: [PlutusV2.PubKeyHash]
listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
-------------------------------------------------------------------------------
-- | Create a token name using a prefix and an integer counter, i.e. token1, token2, etc.
-------------------------------------------------------------------------------
nftName :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
nftName prefix num = prefix <> integerAsByteString num
-------------------------------------------------------------------------------
-- | Create the redeemer data object.
-------------------------------------------------------------------------------
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
      { let a = traceIfFalse "Minting/Burning Error" $ (checkTokenMint && checkOutputDatum 1) || (checkTokenBurn && checkOutputDatum 0) -- mint or burn
      ; let b = traceIfFalse "Signing Tx Error"      $ ContextsV2.txSignedBy info getPkh || checkValidMultisig info listOfPkh 2         -- newm or multisig
      ; let c = traceIfFalse "Invalid Datum Error"   checkInputDatum                                                                    -- input datum equals redeemer
      ; let d = traceIfFalse "Invalid Starter Token" $ Value.geq valueAtValidator tokenValue                                            -- must contain the starter token
      ;         traceIfFalse "Minting Error"         $ all (==True) [a,b,c,d]
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
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
        

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
        Nothing         -> traceIfFalse "No Input Datum Hash" False
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
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
      where 
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt getValidatorHash info
        
    -- the output datum for minting increases the number by one
    checkOutputDatum :: Integer -> Bool
    checkOutputDatum increment = 
      case datumAtValidator of
        Nothing      -> traceIfFalse "No Datum At Validator" False
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