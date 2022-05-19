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
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module NFTLockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  , CustomDatumType
  , Schema
  , contract
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise           ( serialise )
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           Ledger                    hiding ( singleton )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.Contract
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value
import           Data.Aeson                ( FromJSON, ToJSON )
import           Data.OpenApi.Schema       ( ToSchema )
import           GHC.Generics              ( Generic )
import           Prelude                   ( Show )
import CheckFuncs
import TokenHelper
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtNewmPid :: !CurrencySymbol
  , cdtNumber  :: !Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType
-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtNewmPid    a == cdtNewmPid b) &&
           ( cdtNumber a + 1 == cdtNumber  b)
-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
newtype LockingContractParams = LockingContractParams
  { lcpNewmPKH :: PubKeyHash }
PlutusTx.makeLift ''LockingContractParams
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = MintNFT   |
                          Exit
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('MintNFT, 0)
                                                , ('Exit,    1)
                                                ]
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: TypeData -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: LockingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator lcp datum redeemer context =
  case redeemer of
    MintNFT -> do
      { let a = traceIfFalse "Signing Tx Error"    $ txSignedBy info newmPKH
      ; let b = traceIfFalse "Single Script Error" $ isSingleScript txInputs
      ; let c = traceIfFalse "Cont Payin Error"    $ isValueContinuing contOutputs validatingValue
      ; let d = traceIfFalse "NFT Mint Error"      checkMintedAmount
      ; let e = traceIfFalse "Datum Error"         $ isEmbeddedDatum contOutputs
      ;         traceIfFalse "MintNFT Endpoint Error" $ all (==True) [a,b,c,d,e]
      }
    Exit -> do
      { let a = traceIfFalse "Signing Tx Error"    $ txSignedBy info newmPKH
      ;         traceIfFalse "Exit Endpoint Error" $ all (==True) [a]
      }
   where
    info :: TxInfo
    info = scriptContextTxInfo  context

    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs  info

    newmPKH :: PubKeyHash
    newmPKH = lcpNewmPKH lcp

    validatingValue :: Value
    validatingValue =
      case findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> txOutValue $ txInInfoResolved input

    checkMintedAmount :: Bool
    checkMintedAmount =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtNewmPid datum) && (Value.unTokenName tkn == nftName (cdtNumber datum)) && (amt == (1 :: Integer))
        _                -> False

    isEmbeddedDatum :: [TxOut] -> Bool
    isEmbeddedDatum []     = False
    isEmbeddedDatum (x:xs) =
      case txOutDatumHash x of
        Nothing -> isEmbeddedDatum xs
        Just dh ->
          case findDatum dh info of
            Nothing        -> False
            Just (Datum d) ->
              case PlutusTx.fromBuiltinData d of
                Nothing       -> False
                Just embedded -> datum == embedded
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType
-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: LockingContractParams -> Scripts.TypedValidator Typed
typedValidator rpp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode rpp)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer
-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator params)
  where
    params = LockingContractParams { lcpNewmPKH = "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439" }
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV1
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract