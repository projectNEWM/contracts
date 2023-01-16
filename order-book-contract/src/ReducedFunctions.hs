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
module ReducedFunctions
  ( signedBy
  , txInFromTxRef
  , findExactPayout
  , nInputs
  , nOutputs
  ) where
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as V2
-- find the tx in
{-# inlinable txInFromTxRef #-}
txInFromTxRef :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxInInfo
txInFromTxRef [] _ = traceError "Cant Find Tx In"
txInFromTxRef (x:xs) outRef
  | V2.txInInfoOutRef x == outRef = x
  | otherwise                  = txInFromTxRef xs outRef

-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [V2.PubKeyHash] -> V2.PubKeyHash -> Bool
signedBy list k = loop list
  where
    loop [] = False
    loop (x:xs)
      | x == k = True
      | otherwise = loop xs

{-# INLINABLE findExactPayout #-}
findExactPayout :: [V2.TxOut] -> V2.Address -> V2.Value -> Bool
findExactPayout list addr val = helper list
  where
    helper :: [V2.TxOut] -> Bool
    helper [] = False
    helper (x:xs)
      | checkAddr && checkVal = True
      | otherwise             = helper xs
      where
        checkAddr :: Bool
        checkAddr = V2.txOutAddress x == addr

        checkVal :: Bool
        checkVal = V2.txOutValue x == val

-- | Count the number of inputs that have inline datums.
{-# INLINABLE nInputs #-}
nInputs :: [V2.TxInInfo] -> Integer -> Bool
nInputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxInInfo] -> Integer -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case V2.txOutDatum $ V2.txInInfoResolved x of
        V2.NoOutputDatum       -> loopInputs xs   counter
        (V2.OutputDatumHash _) -> loopInputs xs   counter
        (V2.OutputDatum _)     -> loopInputs xs ( counter + 1 ) -- inline

-- | Count the number of outputs that have inline datums.
{-# INLINABLE nOutputs #-}
nOutputs :: [V2.TxOut] -> Integer -> Bool
nOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxOut] -> Integer  -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> loopInputs xs   counter
        (V2.OutputDatumHash _) -> loopInputs xs   counter
        (V2.OutputDatum _)     -> loopInputs xs ( counter + 1 ) -- inline