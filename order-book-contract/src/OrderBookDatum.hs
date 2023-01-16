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
module OrderBookDatum
  ( OwnerInfo (..)
  , TokenInfo (..)
  , createValue
  , checkValueHolds
  , checkMirrorTokens
  , SwapInfo (..)
  , OrderBookDatum (..)
  , TokenSwapInfo (..)
  , checkIfInSlippageRange
  , checkEffectiveSlippage
  , HaveWantInfo (..)
  , checkContValue
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
import           UsefulFuncs            ( isIntegerInRange )
-------------------------------------------------------------------------------
-- | Create the OrderBookData object.
-------------------------------------------------------------------------------
data OwnerInfo = OwnerInfo
  { ptPkh :: V2.PubKeyHash
  -- ^ The public key hash of the owner.
  , ptSc  :: V2.PubKeyHash
  -- ^ The stake credential hash of the owner.
  }
PlutusTx.unstableMakeIsData ''OwnerInfo

-- old == new
instance Eq OwnerInfo where
  {-# INLINABLE (==) #-}
  a == b = ( ptPkh a == ptPkh b ) &&
           ( ptSc  a == ptSc  b )

data TokenInfo = TokenInfo
  { tiPid   :: V2.CurrencySymbol
  -- ^ The policy id.
  , tiTkn   :: V2.TokenName
  -- ^ The token name.
  , tiAmt   :: Integer
  -- ^ The amount of the token.
  }
PlutusTx.unstableMakeIsData ''TokenInfo

createValue :: TokenInfo -> V2.Value
createValue (TokenInfo pid tkn amt) = Value.singleton pid tkn amt

-- | Check if two OrderBookDatums have inverse have and want tokens.
checkMirrorTokens :: TokenInfo -> TokenInfo -> Bool
checkMirrorTokens a b = ( tiPid a == tiPid b ) &&
                        ( tiTkn a == tiTkn b )

checkValueHolds :: TokenInfo -> V2.Value -> Bool
checkValueHolds tkn target = Value.valueOf target (tiPid tkn) (tiTkn tkn) == (tiAmt tkn)

data SwapInfo = SwapInfo
  { siSlippage  :: Integer
  -- ^ The owner allows this much slippage.
  }
PlutusTx.unstableMakeIsData ''SwapInfo

data HaveWantInfo = HaveWantInfo Integer Integer

checkContValue :: HaveWantInfo -> HaveWantInfo -> Bool
checkContValue (HaveWantInfo h1 w1) (HaveWantInfo h2 w2) =
  if leftSide /= rightSide
    then traceError "Bad Trade"
    else leftSide && rightSide
  where
    leftSide :: Bool
    leftSide = (h1 < w2) == True

    rightSide :: Bool
    rightSide = ((w1 < h2) == True)

-- algebraic strucutre to hold one utxo information
data TokenSwapInfo = TokenSwapInfo TokenInfo SwapInfo

-- | Calculate if two order book dats are within their effective slippage range.
checkIfInSlippageRange :: TokenSwapInfo -> TokenSwapInfo -> Bool
checkIfInSlippageRange (TokenSwapInfo tA sA) (TokenSwapInfo tB sB) = (swapA && swapB)
  where
    swapA :: Bool
    swapA = isIntegerInRange (tiAmt tA) aSlip (tiAmt tB) == True

    swapB :: Bool
    swapB = isIntegerInRange (tiAmt tB) bSlip (tiAmt tA) == True
    
    aSlip :: Integer
    aSlip = siSlippage sA

    bSlip :: Integer
    bSlip = siSlippage sB

data OrderBookDatum = Swap OwnerInfo TokenInfo TokenInfo SwapInfo
PlutusTx.unstableMakeIsData ''OrderBookDatum

effectivePrice :: Integer -> Integer -> Integer
effectivePrice haveAmt wantAmt = price
    where
        gammaConstant :: Integer
        gammaConstant = 1000000

        scaledHaveAmt :: Integer
        scaledHaveAmt = gammaConstant * haveAmt

        price :: Integer
        price = divide scaledHaveAmt wantAmt

-- | Calculate if two order book dats are within their effective slippage range.
checkEffectiveSlippage :: OrderBookDatum -> OrderBookDatum -> Bool
checkEffectiveSlippage (Swap _ hA wA sA) (Swap _ hB wB sB) = (swapA && swapB)
  where
    swapA :: Bool
    swapA = isIntegerInRange aPrice aSlip bPrice == True

    swapB :: Bool
    swapB = isIntegerInRange bPrice bSlip aPrice == True

    aPrice :: Integer
    aPrice = effectivePrice (tiAmt hA) (tiAmt wA)

    bPrice :: Integer
    bPrice = effectivePrice (tiAmt hB) (tiAmt wB)

    aSlip :: Integer
    aSlip = siSlippage sA

    bSlip :: Integer
    bSlip = siSlippage sB