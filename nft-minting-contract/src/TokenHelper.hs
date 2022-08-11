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
module TokenHelper
  ( nftName
  ) where

import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api       as PlutusV2
import qualified PlutusTx.Builtins.Internal as Internal

{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Create a token name using a prefix and an integer counter, i.e. token1, token2, etc.
-------------------------------------------------------------------------------
nftName :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
nftName prefix num = prefix <> integerAsString num
-------------------------------------------------------------------------------
-- | The Mapping for converting an integer into a stringed version.
-------------------------------------------------------------------------------
integerToStringMapping :: Integer -> PlutusV2.BuiltinByteString
integerToStringMapping ch
  | ch == 0   = "0"
  | ch == 1   = "1"
  | ch == 2   = "2"
  | ch == 3   = "3"
  | ch == 4   = "4"
  | ch == 5   = "5"
  | ch == 6   = "6"
  | ch == 7   = "7"
  | ch == 8   = "8"
  | ch == 9   = "9"
  | otherwise = ""
-------------------------------------------------------------------------------
-- | Convert an integer into a string.
-------------------------------------------------------------------------------
integerAsString :: Integer -> PlutusV2.BuiltinByteString
integerAsString num = if num == 0 then "0" else convertToString base10 ""
  where
    base10 :: [Integer]
    base10 = baseQ num 10

    convertToString :: [Integer] -> BuiltinByteString -> BuiltinByteString
    convertToString []     str = str
    convertToString (x:xs) str = convertToString xs (str <> integerToStringMapping x)
-------------------------------------------------------------------------------
-- | Write an integer in base Q and return a list of integers.
-------------------------------------------------------------------------------
baseQ :: Integer -> Integer -> [Integer]
baseQ number base = baseQ' number base []
  where
    baseQ' :: Integer -> Integer -> [Integer] -> [Integer]
    baseQ' number' base' list = do
      if number' == 0
      then list
      else baseQ' (Internal.divideInteger number' base') base' (Internal.modInteger number' base' : list)