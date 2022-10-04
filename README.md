# Project NEWM

A collection of smart contracts for Project NEWM.

# v0.2.x

The v0.2.x contract versions assume NEWM holds a special wallet that will regulate token minting and burning. In the future, this centralized signing mechanism will be replaced with a decentralized solution.

### useful-funcs

A library for useful functions for inside the contracts.

https://github.com/logicalmechanism/useful-funcs

## Flow

An artist comes to NEWM wishing to tokenize a piece of art, digitalizing a real-world object onto the blockchain. The artist may decide to split the ownership rights of the tokenized object by fractionalization, splitting a tokeninzed object into 100,000,000 fractions. First, NEWM will create the initial tokenized object by minting an NFT on the official NEWM catalog policy id. At this point, the artist may keep the original tokenized version of their art or they may fractionalize the original NFT into a fungible token.

The fractionalization of a token is the splitting of the ownership rights of the original tokenized art into 100 million pieces. The original tokenized art is locked into a smart contract and the resulting fractions are given to the artist. From here the artist may do as they wish with their fractions.

This entire process is reversible. At any point, an artist may obtain the original tokenized object by burning the 100 million fractional tokens.

## Tokenization

Each UTxO inside the tokenization contract is a unique NEWM catalog, holding a unique datum with a token prefix like "NEWM_", "paintings_", or "songs_" and the current NFT counter. The contract behaves as a sequential NFT generator, minting tokens using the concatenation of the prefix with the current token counter. The sequence of NFTs will continue forever or at least until the size of the token name exceeds 32 characters. The resulting NFTs will have a token name like "NEWM_0" or "NEWM_414".

## Fractionalization

Each UTxO inside the fractionalization contract is a unique fractionalization of a NEWM NFT. The fractionalized tokens will have the fractional NEWM policy id but will share the same name as the original tokenized object. The minting of the fractions will only occur if the original NEWM NFT is sent into the fractional contract to be locked. This means the fractionalized UTxO may only be unlocked if all 100 million fractions are burned together in a single transaction. 


