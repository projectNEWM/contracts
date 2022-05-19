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
  ( integerToHex
  , integerToInteger
  , nftName
  ) where
import           PlutusTx.Prelude
import qualified PlutusTx.Builtins.Internal as Internal

{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
nftName :: BuiltinByteString -> Integer -> BuiltinByteString
nftName prefix num = prefix <> integerToInteger num
-------------------------------------------------------------------------------
intChars :: Integer -> BuiltinByteString
intChars ch
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
  | ch == 10  = "a"
  | ch == 11  = "b"
  | ch == 12  = "c"
  | ch == 13  = "d"
  | ch == 14  = "e"
  | ch == 15  = "f"
  | otherwise = ""


integerToHex :: Integer -> BuiltinByteString
integerToHex num =  convertToHex base16 ""
  where
    base16 :: [Integer]
    base16 = baseQ num 16 []

    convertToHex :: [Integer] -> BuiltinByteString -> BuiltinByteString
    convertToHex [] str = str
    convertToHex (x:xs) str = convertToHex xs (str <> intChars x)

integerToInteger :: Integer -> BuiltinByteString
integerToInteger num =  convertToHex base16 ""
  where
    base16 :: [Integer]
    base16 = baseQ num 10 []

    convertToHex :: [Integer] -> BuiltinByteString -> BuiltinByteString
    convertToHex [] str = str
    convertToHex (x:xs) str = convertToHex xs (str <> intChars x)

baseQ :: Integer -> Integer -> [Integer] -> [Integer]
baseQ number base list = if number == 0 then list else baseQ (Internal.divideInteger number base) base (Internal.modInteger number base : list)