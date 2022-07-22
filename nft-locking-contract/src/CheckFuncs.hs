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
module CheckFuncs
  ( isValueContinuing
  , isSingleScript
  ) where
import qualified Plutus.V1.Ledger.Value as Value
import           Ledger                 hiding ( singleton )
import           PlutusTx.Prelude 
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Search each TxOut for a value.
-------------------------------------------------------------------------------
isValueContinuing :: [TxOut] -> Value -> Bool
isValueContinuing [] _ = False
isValueContinuing (x:xs) val
  | checkVal  = True
  | otherwise = isValueContinuing xs val
  where
    checkVal :: Bool
    checkVal = Value.geq (txOutValue x) val
-------------------------------------------------------------------------------
-- | Force a single script utxo input.
-------------------------------------------------------------------------------
isSingleScript :: [TxInInfo] -> Bool
isSingleScript txInputs = loopInputs txInputs 0
  where
    loopInputs :: [TxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == 1
    loopInputs (x:xs) counter = 
      case txOutDatumHash $ txInInfoResolved x of
        Nothing -> loopInputs xs counter
        Just _  -> loopInputs xs (counter + 1)
