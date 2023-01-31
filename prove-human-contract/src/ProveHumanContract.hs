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
module ProveHumanContract
  ( proveHumanContractScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley                             ( PlutusScript (..)
                                                                 , PlutusScriptV2 )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V1.Ledger.Scripts                        as Scripts
import qualified Plutus.V2.Ledger.Contexts                       as V2
import qualified Plutus.V2.Ledger.Api                            as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           ProveHumanDatum
import           ProveHumanRedeemer
import           GraphFunctions
-- import           ReducedFunctions
-- import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Prove ColorData
  | Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Prove,  0 )
                                                , ( 'Remove, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ProveHumanDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer _ =
  {- | Proof of Humanity Contract

    Prove a wallet is a human via proof of providing the k-coloring of a graph.
  -}
  case (datum, redeemer) of
    (Proof _ _, Remove) -> True -- for testing only

    (Proof _ gStr, Prove cData) -> (giString gStr) == (computeMTree $ constructColoring (nodeList cData) (colorList cData))
    
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: V2.Validator
validator' = V2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

proveHumanContractScriptShortBs :: SBS.ShortByteString
proveHumanContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

proveHumanContractScript :: PlutusScript PlutusScriptV2
proveHumanContractScript = PlutusScriptSerialised proveHumanContractScriptShortBs