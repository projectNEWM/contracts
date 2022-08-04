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
module V2CheckFuncs
  ( isValueContinuing
  , isNScripts
  , isVoteComplete
  , createBuiltinByteString
  , createAddress
  ) where
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Value      as Value
-- import qualified Plutus.V2.Ledger.Contexts   as ContextsV2
import qualified Plutus.V2.Ledger.Api        as PlutusV2
import           PlutusTx.Prelude 
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------
-- | Create a proper Address Type.
-------------------------------------------------------------------------
createAddress :: PlutusV2.PubKeyHash -> PlutusV2.PubKeyHash -> PlutusV2.Address
createAddress pkh sc = if PlutusV2.getPubKeyHash sc == emptyByteString then PlutusV2.Address (PubKeyCredential pkh) Nothing else PlutusV2.Address (PubKeyCredential pkh) (Just $ StakingHash $ PubKeyCredential sc)
-------------------------------------------------------------------------
-- | Check if the total value contains the threshold value of a token.
-- Add in a way to account for value on the reference inputs that someone signed for.
-------------------------------------------------------------------------

isVoteComplete :: PlutusV2.CurrencySymbol -> PlutusV2.TokenName -> Integer -> PlutusV2.TxInfo -> PlutusV2.Value -> Bool
isVoteComplete pid tkn amt info val = do -- remove in production
      { let a = traceIfFalse "Not Enough Vote Tokens"    $ Value.geq totalValue thresholdValue
      ;         traceIfFalse "Vote Audit Endpoint Error" $ all (==True) [a]
      }
  where
    valueSpentTxInputs' :: PlutusV2.TxInfo -> PlutusV2.Value
    valueSpentTxInputs' = (foldMap (PlutusV2.txOutValue . PlutusV2.txInInfoResolved) . PlutusV2.txInfoInputs)

    totalValue :: PlutusV2.Value
    totalValue = (valueSpentTxInputs' info) + val -- total value of inputs and ref inputs

    thresholdValue :: PlutusV2.Value
    thresholdValue = Value.singleton pid tkn amt
-------------------------------------------------------------------------
-- | Appends two bytestrings together from a list, element by element
-------------------------------------------------------------------------
flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
flattenBuiltinByteString [] = emptyByteString 
flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString.
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString |x <- intList]
-------------------------------------------------------------------------------
-- | Search each TxOut for a value.
-------------------------------------------------------------------------------
isValueContinuing :: [PlutusV2.TxOut] -> PlutusV2.Value -> Bool
isValueContinuing [] _ = False
isValueContinuing (x:xs) val
  | checkVal  = True
  | otherwise = isValueContinuing xs val
  where
    checkVal :: Bool
    checkVal = Value.geq (PlutusV2.txOutValue x) val
-------------------------------------------------------------------------------
-- | Force a single script utxo input.
-------------------------------------------------------------------------------
isNScripts :: [PlutusV2.TxInInfo] -> Integer -> Bool
isNScripts txInputs value' = loopInputs txInputs 0
  where
    loopInputs :: [PlutusV2.TxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == value'
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)
