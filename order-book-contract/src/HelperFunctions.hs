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
module HelperFunctions
  ( effectivePrice
  , createTxOutRef
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
-------------------------------------------------------------------------------
-- | Calculate ratio price.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Calculate the effective price.
-------------------------------------------------------------------------------
effectivePrice :: Integer -> Integer -> Integer
effectivePrice haveAmt wantAmt = price
    where
        gammaConstant :: Integer
        gammaConstant = 1000000

        scaledHaveAmt :: Integer
        scaledHaveAmt = gammaConstant * haveAmt

        price :: Integer
        price = divide scaledHaveAmt wantAmt
-------------------------------------------------------------------------------
-- | Create a TxOutRef from the tx hash and index.
-------------------------------------------------------------------------------
createTxOutRef :: PlutusV2.TxId -> Integer -> PlutusV2.TxOutRef
createTxOutRef txHash index = txId
  where
    txId :: PlutusV2.TxOutRef
    txId = PlutusV2.TxOutRef
      { PlutusV2.txOutRefId  = txHash
      , PlutusV2.txOutRefIdx = index
      }