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
module GraphFunctions
  ( createGraph
  , createIsomorphism
  , isomorphicGraph
  , kColoring
  , computeMTree
  , constructColoring
  ) where
-- import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as V2
import           UsefulFuncs
-------------------------------------------------------------------------------
-- | Create a graph from a bytestring
-------------------------------------------------------------------------------
{-# INLINABLE createGraph #-}
createGraph :: V2.BuiltinByteString -> [(Integer, [Integer])]
createGraph startString =
  if lenList == 0 || even lenList == False
    then [] 
    else group $ createGraph' 0 intList []
  where
    intList :: [Integer]
    intList = byteStringAsIntegerList startString

    lenList :: Integer
    lenList = lengthOfByteString startString

    halfLenList :: Integer
    halfLenList = divide lenList 2

    createGraph' :: Integer -> [Integer] -> [(Integer, [Integer])] -> [(Integer, [Integer])]
    createGraph' _       []     holder = holder
    createGraph' counter (x:xs) holder = createGraph' (counter + 1) xs (addToList newNodeTuple holder)
      where
        newNodeTuple :: (Integer, [Integer])
        newNodeTuple = (counter, removeDuplicates $ (filter (/= counter) (baseQ x halfLenList)))

        addToList :: (Eq a) => a -> [a] -> [a]
        addToList x' list = if elem x' list then list else x':list

        removeDuplicates :: [Integer] -> [Integer]
        removeDuplicates = foldr (\x' acc -> if x' `elem` acc then acc else x':acc) []

    group :: [(Integer, [Integer])] -> [(Integer, [Integer])]
    group xs = map (\(a,b) -> (a, b <> findRefs a b xs)) xs
      where
        findRefs :: Integer -> [Integer] -> [(Integer, [Integer])] -> [Integer]
        findRefs a refs xs = map fst (filter (\(x, y) -> x /= a && (elem a y) && not (elem x refs)) xs)
-------------------------------------------------------------------------------
-- | Create an isomorphic graph from a string input.
-------------------------------------------------------------------------------
{-# INLINABLE createIsomorphism #-}
createIsomorphism :: V2.BuiltinByteString -> [Integer]
createIsomorphism stringInput =
  if lenList == 0 || even lenList == False
    then [] 
    else createIsoMap stringInput []
  where
    lenList :: Integer
    lenList = lengthOfByteString stringInput

    halfLenList :: Integer
    halfLenList = divide lenList 2

    createIsoMap :: V2.BuiltinByteString -> [Integer] -> [Integer]
    createIsoMap stringInput' holder =
      if length holder == halfLenList
        then holder
        else 
          let intList = [modulo n halfLenList | n <- byteStringAsIntegerList stringInput' ]
              holder' = checkIfContainsAll intList holder
          in createIsoMap (hash stringInput') holder'
      where 
        checkIfContainsAll :: [Integer] -> [Integer] -> [Integer]
        checkIfContainsAll []     ys = ys
        checkIfContainsAll (x:xs) ys
          | x `elem` ys = checkIfContainsAll xs ys
          | otherwise = checkIfContainsAll xs (x:ys)
-------------------------------------------------------------------------------
-- | Morph the graph
-------------------------------------------------------------------------------
{-# INLINABLE isomorphicGraph #-}
isomorphicGraph :: [(Integer, [Integer])] -> [Integer] -> [(Integer, [Integer])]
isomorphicGraph graph isomorphism = isomorphicGraph' graph [] isomorphism
  where
    isomorphicGraph' :: [(Integer, [Integer])] -> [(Integer, [Integer])] -> [Integer] -> [(Integer, [Integer])]
    isomorphicGraph' []                    newGraph _            = reverse newGraph
    isomorphicGraph' ((oldNode, edges):xs) newGraph isomorphism' = isomorphicGraph' xs ((isomorphism' !! oldNode, morphEdges edges [] isomorphism'):newGraph) isomorphism'

    morphEdges :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    morphEdges []     holder _            = reverse holder
    morphEdges (x:xs) holder isomorphism' = morphEdges xs ((isomorphism' !! x):holder) isomorphism'
-------------------------------------------------------------------------------
-- | This function colors a given graph with a minimum number of colors.
-------------------------------------------------------------------------------
{-# INLINABLE kColoring #-}
kColoring :: [(Integer, [Integer])] -> [(Integer, Integer)]
kColoring graph = kColoring' graph (length graph) []

kColoring' :: [(Integer, [Integer])] -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
kColoring' [] _ coloring = coloring
kColoring' ((node, edges):graph) maxColor coloring = kColoring' graph maxColor ((node, color):coloring)
  where
    color :: Integer
    color = head [c | c <- [0..maxColor-1], not (nodeColored (map (\n -> (n,c)) edges) coloring)]

    nodeColored :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
    nodeColored n coloring = any (\x -> elem x n) coloring
-------------------------------------------------------------------------------
-- | Computes the merkle tree of a unique graph coloring
-------------------------------------------------------------------------------
{-# INLINABLE computeMTree #-}
computeMTree :: [(Integer, Integer)] -> V2.BuiltinByteString
computeMTree coloring = merkleTree usedColorStrings
  where
    usedColorStrings :: [V2.BuiltinByteString]
    usedColorStrings = [integerAsByteString g <> integerAsByteString c | (g, c) <- coloring]
-------------------------------------------------------------------------------
-- | Recontruct a graph coloring from two lists of integers.
-------------------------------------------------------------------------------
{-# INLINABLE constructColoring #-}
constructColoring :: [Integer] -> [Integer] -> [(Integer, Integer)]
constructColoring nodes colors = constructColoring' nodes colors []
  where
    constructColoring' :: [Integer] -> [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
    constructColoring' []     []     holder = holder
    constructColoring' _      []     _      = []
    constructColoring' []     _      _      = []
    constructColoring' (x:xs) (y:ys) holder = constructColoring' xs ys ((x,y):holder)