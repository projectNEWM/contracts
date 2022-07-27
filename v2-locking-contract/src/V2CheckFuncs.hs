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
  , isAddrGettingPaid
  , isSingleScript
  , createAddress
  , createBuiltinByteString
  ) where
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Value      as Value
import qualified Plutus.V2.Ledger.Api        as PlutusV2
import           PlutusTx.Prelude 
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
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
-------------------------------------------------------------------------
-- | Create a proper Address Type.
-------------------------------------------------------------------------
createAddress :: PlutusV2.PubKeyHash -> PlutusV2.PubKeyHash -> PlutusV2.Address
createAddress pkh sc = if PlutusV2.getPubKeyHash sc == emptyByteString then PlutusV2.Address (PubKeyCredential pkh) Nothing else PlutusV2.Address (PubKeyCredential pkh) (Just $ StakingHash $ PubKeyCredential sc)
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
-- | Search each TxOut for an pkh and value.
-------------------------------------------------------------------------------
isAddrGettingPaid :: [PlutusV2.TxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaid []     _    _ = False
isAddrGettingPaid (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaid xs addr val
  where
    checkAddr :: Bool
    checkAddr = PlutusV2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.geq (PlutusV2.txOutValue x) val
-------------------------------------------------------------------------------
-- | Force a single script utxo input.
-------------------------------------------------------------------------------
isSingleScript :: [PlutusV2.TxInInfo] -> Bool
isSingleScript txInputs = loopInputs txInputs 0
  where
    loopInputs :: [PlutusV2.TxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == 1
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)
