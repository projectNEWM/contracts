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
  , IncreaseADA   (..)
  , SwapData      (..)
  , checkNewOrderBookData
  , checkIfPartialSwap
  , checkMirrorredDatums
  , verifyExtraADA
  , checkIfInSlippageRange
  , checkIfInEffectiveSlippageRange
  -- , calculateRatioPrice
  -- testing below
  , testData1
  , testData2
  , testData3
  , testData4
  , showTestAmts
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
import           UsefulFuncs             ( pow
                                         , isIntegerInRange
                                         )
import           HelperFunctions         ( effectivePrice )
-------------------------------------------------------------------------------
-- | Create the OrderBookData object.
-------------------------------------------------------------------------------
data OwnerInfo = OwnerInfo
  { ptPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the owner.
  , ptSc  :: PlutusV2.PubKeyHash
  -- ^ The stake credential hash of the owner.
  }
PlutusTx.unstableMakeIsData ''OwnerInfo

-- old == new
instance Eq OwnerInfo where
  {-# INLINABLE (==) #-}
  a == b = ( ptPkh a == ptPkh b ) &&
           ( ptSc  a == ptSc  b )

data TokenInfo = TokenInfo
  { tiPid   :: PlutusV2.CurrencySymbol
  -- ^ The owner has policy id.
  , tiTkn   :: PlutusV2.TokenName
  -- ^ The owner has token name.
  , tiAmt   :: Integer
  -- ^ The owner has this amount.
  }
PlutusTx.unstableMakeIsData ''TokenInfo

data SwapInfo = SwapInfo
  { siSlippage  :: Integer
  -- ^ The owner allows this much slippage.
  , siFeeAmt    :: Integer
  -- ^ The owner is willing to pay this much fee.
  , siIncentive :: Integer
  -- ^ The owner is willing to pay this much incentive.
  }
PlutusTx.unstableMakeIsData ''SwapInfo

data OrderBookDatum = Swap OwnerInfo TokenInfo TokenInfo SwapInfo
PlutusTx.unstableMakeIsData ''OrderBookDatum

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

-- old == new
instance Eq OrderBookData where
  {-# INLINABLE (==) #-}
  a == b = ( obPkh       a == obPkh       b ) &&
           ( obSc        a == obSc        b ) &&
           ( obHavePid   a == obHavePid   b ) &&
           ( obHaveTkn   a == obHaveTkn   b ) &&
           ( obHaveAmt   a == obHaveAmt   b ) &&
           ( obWantPid   a == obWantPid   b ) &&
           ( obWantTkn   a == obWantTkn   b ) &&
           ( obWantAmt   a == obWantAmt   b ) &&
           ( obSlippage  a == obSlippage  b ) &&
           ( obFeeAmt    a == obFeeAmt    b ) &&
           ( obIncentive a == obIncentive b )

-- | Check the update on the order book is only changing the want token, slip, fee, and incentive information.
checkNewOrderBookData :: OrderBookData -> OrderBookData -> Bool
checkNewOrderBookData a b = ( obPkh     a == obPkh     b ) &&
                            ( obSc      a == obSc      b ) &&
                            ( obHavePid a == obHavePid b ) &&
                            ( obHaveTkn a == obHaveTkn b ) &&
                            ( obHaveAmt a == obHaveAmt b )

-- | Check if two datums have inverse have and want tokens.
checkMirrorredDatums :: OrderBookData -> OrderBookData -> Bool
checkMirrorredDatums a b =  ( obHavePid a == obWantPid b ) &&
                            ( obHaveTkn a == obWantTkn b )

-- | Check if two datums are a full swap or a partial swap
checkIfPartialSwap :: OrderBookData -> OrderBookData -> Integer
checkIfPartialSwap thisDatum thatDatum =
  if checkIfInSlippageRange thisDatum thatDatum
    then 0 -- full swap
    else 1 -- partial swap

-- | Calculate if two order book dats are within their effective slippage range.
checkIfInSlippageRange :: OrderBookData -> OrderBookData -> Bool
checkIfInSlippageRange a b =  isIntegerInRange (obHaveAmt a) aSlip (obWantAmt b) == True && 
                              isIntegerInRange (obHaveAmt b) bSlip (obWantAmt a) == True
  where
    aSlip :: Integer
    aSlip = obSlippage a

    bSlip :: Integer
    bSlip = obSlippage b

-- | Calculate if two order book dats are within their effective slippage range.
checkIfInEffectiveSlippageRange :: OrderBookData -> OrderBookData -> Bool
checkIfInEffectiveSlippageRange a b =  isIntegerInRange aPrice aSlip bPrice == True && 
                                       isIntegerInRange bPrice bSlip aPrice == True
  where
    aPrice :: Integer
    aPrice = effectivePrice (obHaveAmt a) (obWantAmt a)

    bPrice :: Integer
    bPrice = effectivePrice (obWantAmt b) (obHaveAmt b)

    aSlip :: Integer
    aSlip = obSlippage a

    bSlip :: Integer
    bSlip = obSlippage b

-------------------------------------------------------------------------------
-- | TEST DATA
-------------------------------------------------------------------------------
-- | Test data for price calculations
testData1 :: OrderBookData
testData1 = OrderBookData
  { obPkh       = ""
  , obSc        = ""
  , obHavePid   = ""
  , obHaveTkn   = ""
  , obHaveAmt   = 617283
  , obWantPid   = ""
  , obWantTkn   = ""
  , obWantAmt   = 555555
  , obSlippage  = 20
  , obFeeAmt    = 0
  , obIncentive = 0
  }

testData2 :: OrderBookData
testData2 = OrderBookData
  { obPkh       = ""
  , obSc        = ""
  , obHavePid   = ""
  , obHaveTkn   = ""
  , obHaveAmt   = 541623
  , obWantPid   = ""
  , obWantTkn   = ""
  , obWantAmt   = 630412
  , obSlippage  = 20
  , obFeeAmt    = 0
  , obIncentive = 0
  }

testData3 :: OrderBookData
testData3 = OrderBookData
  { obPkh       = ""
  , obSc        = ""
  , obHavePid   = ""
  , obHaveTkn   = ""
  , obHaveAmt   = 1220824
  , obWantPid   = ""
  , obWantTkn   = ""
  , obWantAmt   = 1083246
  , obSlippage  = 20
  , obFeeAmt    = 0
  , obIncentive = 0
  }

testData4 :: OrderBookData
testData4 = OrderBookData
  { obPkh       = ""
  , obSc        = ""
  , obHavePid   = ""
  , obHaveTkn   = ""
  , obHaveAmt   = 1083246
  , obWantPid   = ""
  , obWantTkn   = ""
  , obWantAmt   = 1220824
  , obSlippage  = 20
  , obFeeAmt    = 0
  , obIncentive = 0
  }

showTestAmts :: OrderBookData -> [(Integer, Integer, Integer)]
showTestAmts a = [(obHaveAmt a, obWantAmt a, effectivePrice (obHaveAmt a) (obWantAmt a))]

-- calculateRatioPrice :: OrderBookData -> OrderBookData -> Integer
-- calculateRatioPrice a b = price
--   where

--     h1 = obHaveAmt a
--     w1 = obWantAmt a

--     h2 = obHaveAmt b
--     w2 = obWantAmt b

--     price = divide (h1*w2) h2
--     -- price = divide (h2*w1) h1


-------------------------------------------------------------------------------
-- | Create the IncreaseADA object.
-------------------------------------------------------------------------------
data IncreaseADA = IncreaseADA
  { iExtraAda     :: Integer
  -- ^ The extra ada required for the min ada.
  , iExtraAdaFlag :: Integer
  -- ^ The positive/negative flag for min ada.
  , iFeeDiff      :: Integer
  -- ^ The difference in the fee that is being added in the tx.
  , iFeeDiffFlag  :: Integer
  -- ^ The positive/negative flag for fee diff.
  , iIncDiff      :: Integer
  -- ^ The difference in the incentive being added in the tx.
  , iIncDiffFlag  :: Integer
  -- ^ The positive/negative flag for incentive diff.
  }
PlutusTx.unstableMakeIsData ''IncreaseADA

-- confirm that the fee and incentive have been increased properly
verifyExtraADA :: OrderBookData -> OrderBookData -> IncreaseADA -> Bool
verifyExtraADA a b c =  ( obFeeAmt    a + (pow (-1) (iFeeDiffFlag c)) * (iFeeDiff c) == obFeeAmt    b ) &&
                        ( obIncentive a + (pow (-1) (iIncDiffFlag c)) * (iIncDiff c) == obIncentive b )
-------------------------------------------------------------------------------
-- | Create the SwapData object.
-------------------------------------------------------------------------------
data SwapData = SwapData
  { sTx  :: PlutusV2.BuiltinByteString
  -- ^ The tx hash of the other utxo being swapped.
  , sIdx :: Integer
  -- ^ The index of the tx hash.
  }
PlutusTx.unstableMakeIsData ''SwapData