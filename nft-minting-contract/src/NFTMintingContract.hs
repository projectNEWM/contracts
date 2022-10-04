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
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [254, 67, 217, 228, 63, 221, 205, 93, 162, 223, 115, 214, 63, 237, 245, 3, 134, 124, 239, 37, 53, 223, 12, 139, 77, 213, 19, 64] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [78, 69, 87, 77, 95] }

-- check for nft here
tokenValue :: PlutusV2.Value
tokenValue = Value.singleton lockPid lockTkn (1 :: Integer)

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [244, 112, 29, 51, 244, 194, 108, 51, 221, 146, 159, 57, 198, 163, 232, 159, 252, 188, 248, 57, 77, 44, 193, 244, 2, 217, 138, 196]

getPkh :: PlutusV2.PubKeyHash
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [85, 15, 135, 50, 248, 57, 226, 178, 62, 195, 209, 187, 50, 138, 78, 21, 190, 219, 187, 32, 171, 156, 113, 43, 92, 241, 26, 217] }

-- collat wallet
multiPkh1 :: PlutusV2.PubKeyHash
multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [111, 255, 109, 51, 149, 143, 101, 2, 45, 230, 74, 171, 211, 193, 106, 122, 126, 7, 186, 215, 169, 74, 69, 133, 206, 29, 172, 118] }
-- seller wallet
multiPkh2 :: PlutusV2.PubKeyHash
multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [63, 22, 163, 63, 160, 112, 117, 22, 247, 235, 164, 210, 223, 197, 124, 214, 18, 122, 160, 94, 171, 114, 217, 90, 27, 249, 82, 99] }
-- reference wallet
multiPkh3 :: PlutusV2.PubKeyHash
multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [60, 47, 234, 52, 123, 28, 154, 240, 143, 240, 62, 109, 37, 231, 123, 240, 32, 118, 204, 101, 205, 133, 30, 131, 27, 182, 139, 132] }

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