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
  , findPayout
  , nInputs
  , nOutputs
  , nRedeemers
  , getScriptOutputs
  , ownInput
  ) where
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api      as V2
import qualified Plutus.V1.Ledger.Value    as Value


{-# inlinable getScriptOutputs #-}
getScriptOutputs :: [V2.TxOut] -> V2.Address -> [V2.TxOut]
getScriptOutputs txOuts addr' = getScriptOutputs' txOuts addr' []
  where
    getScriptOutputs' :: [V2.TxOut] -> V2.Address -> [V2.TxOut] -> [V2.TxOut]
    getScriptOutputs' [] _ contOuts = contOuts
    getScriptOutputs' (x:xs) addr contOuts
      | V2.txOutAddress x == addr = getScriptOutputs' xs addr (x:contOuts)
      | otherwise                 = getScriptOutputs' xs addr contOuts

-- rewrite findOwnInput without higher order functions
{-# inlinable ownInput #-}
ownInput :: V2.ScriptContext -> V2.TxOut
ownInput (V2.ScriptContext t_info (V2.Spending o_ref)) = getScriptInput (V2.txInfoInputs t_info) o_ref
ownInput _ = traceError "no script input"

-- get the validating script input
{-# inlinable getScriptInput #-}
getScriptInput :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((V2.TxInInfo tref ot) : xs) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput xs o_ref

{-# inlinable txInFromTxRef #-}
txInFromTxRef :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxInInfo
txInFromTxRef txIns outRef = txInFromTxRef' txIns
  where
    txInFromTxRef' :: [V2.TxInInfo] -> V2.TxInInfo
    txInFromTxRef' [] = traceError "Cant Find Tx In"
    txInFromTxRef' (x:xs)
      | V2.txInInfoOutRef x == outRef = x
      | otherwise                     = txInFromTxRef' xs

-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [V2.PubKeyHash] -> V2.PubKeyHash -> Bool
signedBy list k = loop list
  where
    loop [] = False
    loop (x:xs)
      | x == k = True
      | otherwise = loop xs

{-# INLINABLE findPayout #-}
findPayout :: [V2.TxOut] -> V2.Address -> V2.Value -> Bool
findPayout list addr val = helper list
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
        checkVal = Value.geq (V2.txOutValue x) val

-- | Count the number of inputs that have inline datums.
{-# INLINABLE nInputs #-}
nInputs :: [V2.TxInInfo] -> V2.Address -> Integer -> Bool
nInputs utxos addr number = loopInputs utxos 0 0
  where
    loopInputs :: [V2.TxInInfo] -> Integer -> Integer -> Bool
    loopInputs []     !dC !sC = (dC == number) && (sC == number)
    loopInputs (x:xs) !dC !sC = 
      case V2.txOutDatum txInOut  of
        V2.NoOutputDatum       -> loopInputs xs dC sC
        (V2.OutputDatumHash _) -> loopInputs xs dC sC
        (V2.OutputDatum _)     -> 
          if V2.txOutAddress txInOut == addr
            then loopInputs xs (dC + 1) (sC + 1) -- inline
            else loopInputs xs (dC + 1) sC
      
      where 

        txInOut :: V2.TxOut
        txInOut = V2.txInInfoResolved x

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

{-# INLINABLE nRedeemers #-}
nRedeemers :: [(V2.ScriptPurpose, V2.Redeemer)] -> Integer -> Bool
nRedeemers redeemers number = nRedeemers' redeemers 0
  where
    nRedeemers' :: [(V2.ScriptPurpose, V2.Redeemer)] -> Integer -> Bool
    nRedeemers' []     counter = counter == number
    nRedeemers' ((_, _):xs) counter = nRedeemers' xs ( counter + 1 )
    -- nRedeemers' _ _ = traceError "Wrong Type Of Redeemer Being Used"