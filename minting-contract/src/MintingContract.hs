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
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Plutus.V1.Ledger.Value   as PV
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx
import           Codec.Serialise
{-
  Author: Quinn Parkinson
  Rev: 0
-}
newtype MintParams = MintParams
  { mpNewmPKH :: PubKeyHash }
PlutusTx.makeLift ''MintParams
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp _ context = checkMintedAmount && checkSigner
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    checkSigner :: Bool
    checkSigner = traceIfFalse "Incorrect Signer" $ txSignedBy info (mpNewmPKH mp)

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, _, _)] -> cs  == ownCurrencySymbol context
      _            -> traceIfFalse "Incorrect Policy Id" False

-------------------------------------------------------------------------------
policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
-------------------------------------------------------------------------------
plutusScript :: Script
plutusScript = unMintingPolicyScript (policy $ MintParams { mpNewmPKH = "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439"})

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