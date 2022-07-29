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
module V2VotingContract
  ( votingContractScript
  , votingContractScriptShortBs
  , CustomDatumType
  , getPkh
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           V2CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}

getPkh :: PlutusV2.PubKeyHash -- remove in production
getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [162, 16, 139, 123, 23, 4, 249, 254, 18, 201, 6, 9, 110, 161, 99, 77, 248, 224, 137, 201, 204, 253, 101, 26, 186, 228, 164, 57] }

-- starter nft
startPid :: PlutusV2.CurrencySymbol
startPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString []}

startTkn :: PlutusV2.TokenName
startTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString []}

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtPid :: PlutusV2.CurrencySymbol
    -- ^ The voting token's policy id
    , cdtTkn :: PlutusV2.TokenName
    -- ^ The voting token's token name.
    , cdtAmt :: Integer
    -- ^ The voting token's threshold amount.
    }
PlutusTx.unstableMakeIsData ''CustomDatumType
-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtPid a == cdtPid b ) &&
           ( cdtTkn a == cdtTkn b ) &&
           ( cdtAmt a /= cdtAmt b ) -- should this be greater than or some bound function?
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Vote   |
                          Exit
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Vote,   0)
                                                , ('Exit,   1)
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Vote -> do 
      { let a = traceIfFalse "Vote Has Failed"           $ isVoteComplete (cdtPid datum) (cdtTkn datum) (cdtAmt datum) info 
      ; let b = traceIfFalse "Single Script Input Error" $ isSingleScript txInputs
      ; let c = traceIfFalse "Missing Starter NFT Error" $ Value.geq validatingValue starterNFT
      ; let d = traceIfFalse "Datum Update Error"        $ isEmbeddedDatum contOutputs
      ; let e = traceIfFalse "Value Not Continuing"      $ isValueContinuing contOutputs validatingValue
      ;         traceIfFalse "Vote Endpoint Error"       $ all (==True) [a,b,c,d,e]
      }
    Exit -> do -- remove in production or make into an update endpoint
      { let a = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info getPkh
      ;         traceIfFalse "Exit Endpoint Error" $ all (==True) [a]
      }
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    -- continue / inputs
    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- the intialization nft to ensure uniqueness
    starterNFT :: PlutusV2.Value
    starterNFT = Value.singleton startPid startTkn (1 :: Integer)
    
    -- check if the outgoing datum has the correct form.
    isEmbeddedDatum :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatum []     = False
    isEmbeddedDatum (x:xs) = 
      case PlutusV2.txOutDatum x of
        -- datumless
        PlutusV2.NoOutputDatum -> isEmbeddedDatum xs
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> isEmbeddedDatum xs
            Just inline -> datum == inline
        -- embedded datum
        (PlutusV2.OutputDatumHash dh) -> 
          case ContextsV2.findDatum dh info of
            Nothing                  -> isEmbeddedDatum xs
            Just (PlutusV2.Datum d') -> 
              case PlutusTx.fromBuiltinData d' of
                Nothing       -> isEmbeddedDatum xs
                Just embedded -> datum == embedded
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

votingContractScriptShortBs :: SBS.ShortByteString
votingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

votingContractScript :: PlutusScript PlutusScriptV2
votingContractScript = PlutusScriptSerialised votingContractScriptShortBs