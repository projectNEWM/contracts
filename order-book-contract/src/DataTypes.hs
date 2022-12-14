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
module DataTypes
  ( OrderBookData (..)
  , IncreaseADA (..)
  , updateOrderBookData
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
-------------------------------------------------------------------------------
-- | Create the OrderBookData object.
-------------------------------------------------------------------------------
data OrderBookData = OrderBookData
  { obPkh       :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the owner.
  , obSc        :: PlutusV2.PubKeyHash
  -- ^ The stake credential hash of the owner.
  , obHavePid   :: PlutusV2.CurrencySymbol
  -- ^ The owner has policy id.
  , obHaveTkn   :: PlutusV2.TokenName
  -- ^ The owner has token name.
  , obHaveAmt   :: Integer
  -- ^ The owner has this amount.
  , obWantPid   :: PlutusV2.CurrencySymbol
  -- ^ The owner wants this policy id.
  , obWantTkn   :: PlutusV2.TokenName
  -- ^ The owner wants this token name.
  , obWantAmt   :: Integer
  -- ^ The owner wants this amount.
  , obSlippage  :: Integer
  -- ^ The owner allows this much slippage.
  , obFeeAmt    :: Integer
  -- ^ The owner is willing to pay this much fee.
  , obIncentive :: Integer
  -- ^ The owner is willing to pay this much incentive.
  }
PlutusTx.unstableMakeIsData ''OrderBookData

-- | Update the order book want token, slip, fee, and incentive information.
updateOrderBookData :: OrderBookData -> OrderBookData -> Bool
updateOrderBookData a b = ( obPkh     a == obPkh     b ) &&
                          ( obSc      a == obSc      b ) &&
                          ( obHavePid a == obHavePid b ) &&
                          ( obHaveTkn a == obHaveTkn b ) &&
                          ( obHaveAmt a == obHaveAmt b )
-------------------------------------------------------------------------------
-- | Create the IncreaseADA object.
-------------------------------------------------------------------------------
data IncreaseADA = IncreaseADA
  { iExtraAda :: Integer
  -- ^ The lovelace amount for the printer payout.
  }
PlutusTx.unstableMakeIsData ''IncreaseADA