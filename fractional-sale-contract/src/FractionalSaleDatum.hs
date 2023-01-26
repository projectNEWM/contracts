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
module FractionalSaleDatum
  ( OwnerInfo (..)
  , TokenInfo (..)
  , multiplyValue
  , checkEmptyTokenInfo
  , FractionalSaleDatum (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
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

-- old == new
instance Eq TokenInfo where
  {-# INLINABLE (==) #-}
  a == b = ( tiPid a == tiPid b ) &&
           ( tiTkn a == tiTkn b ) &&
           ( tiAmt a == tiAmt b )

checkEmptyTokenInfo :: TokenInfo -> Bool
checkEmptyTokenInfo (TokenInfo _ _ amt) = amt > 0

multiplyValue :: TokenInfo -> Integer -> V2.Value
multiplyValue (TokenInfo pid tkn amt) mul = Value.singleton pid tkn (amt*mul)

-- owner, token for a bundle, cost for a bundle
data FractionalSaleDatum = Sale OwnerInfo TokenInfo TokenInfo
PlutusTx.unstableMakeIsData ''FractionalSaleDatum
