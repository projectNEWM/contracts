{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MintingContract
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Ledger                   hiding (singleton)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
-- import           Prelude                  (IO, Semigroup (..), Show (..), String, snd)
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
-- import           Plutus.V1.Ledger.Time    (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import           Plutus.V1.Ledger.Value   as PV
-- import           Ledger.Interval          (after, lowerBound)
-- import           PlutusTx                 (Data (..))
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Ledger.Typed.Scripts     as Scripts
-- import           Ledger.Value             as Value
-- import           Ledger.Ada               as Ada
import qualified PlutusTx
import           Codec.Serialise
-- import           Ledger.TimeSlot
-- import qualified Data.Text                as T
-- import           Data.Text.Encoding        as Text (decodeUtf8, encodeUtf8)
-- import           PlutusTx.Builtins
{-

  Author: Quinn Parkinson

-}
data MintParams = MintParams {}
PlutusTx.makeLift ''MintParams
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy _ _ context = checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, _, amt)] -> cs  == ownCurrencySymbol context && amt == (100000000 :: Integer)
      _              -> traceIfFalse "Minting was done incorrectly." False

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript (policy $ MintParams {})

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