# Project NEWM

A collection of smart contracts for Project NEWM.

# V2.0.0 (Aiken)

- TODO

### assist lib

A library of specialized Aiken functions for smart contracts on Cardano.

https://github.com/logicalmechanism/assist

## Flow

- TODO

## Fractionalization With CIP68

- TODO

## Building

```bash
./complete_build.sh  
```

## Testing

```bash
aiken check
```

# v1.0.0 (PlutusV2)

The v1.0.0 plutus contract version assume NEWM holds a special wallet that will regulate the minting of tokenized tokens and minting and burning fractionalized tokens. A multisig regulates the burning of tokenized tokens.

### useful-funcs

A library for useful functions for inside the contracts.

https://github.com/logicalmechanism/useful-funcs

## Flow

An artist comes to NEWM wishing to tokenize a piece of art, digitalizing a real-world object onto the blockchain. The artist may decide to split the ownership rights of the tokenized object by fractionalization, splitting a tokeninzed object into 100,000,000 fractions. First, NEWM will create the initial tokenized object by minting an NFT on an official NEWM catalog policy id. At this point, the artist may keep the original tokenized version of their art or they may fractionalize the original NFT into a fungible token.

Fractionalization refers to the process of dividing the ownership of streaming rights of a tokenized piece of art into 100 million pieces, locking the tokenized token into a smart contract. The resulting fractions are given to the artist, who has the freedom to sell or trade them as they wish, or are sold on the NEWM marketplace with the proceeds going directly to the artist.

The process is reversible. At any point, a wallet may obtain the original tokenized token by burning the 100 million fractional tokens.

## Tokenization

Each UTxO contained within the tokenization contract holding a starter token functions as a catalog for NEWM. The catalog includes a datum with a token prefix, such as "NEWM", "Paintings", or "Songs", as well as the current NFT counter. As a sequential NFT generator, the contract mints tokens by combining the prefix, an underscore, and the current token counter. This sequence of NFTs is infinite, or at least until the token name exceeds 32 characters. Therefore, the resulting NFTs will be named in the format "NEWM_0" or "NEWM_414", and so on.

## Fractionalization

Within the fractionalization contract, each UTxO represents a distinct fractionalization of a NEWM tokenized token. These fractionalized tokens will have the fractional NEWM policy ID, but will retain the same name as the original tokenized object. To mint the fractions, the original NEWM NFT must first be sent into the fractional contract to be locked. As a result, the fractionalized UTxO can only be unlocked if all 100 million fractions are burned together in a single transaction.

## Building

Compiling the smart contracts is a simple as running the script `complete_build.sh` with a specific prefix.

```bash
./complete_build.sh NEWM
```

This script will populate the testing folder with the update-to-date contracts.

## Testing

The testing of the smart contracts are split into two steps located in the `test-suite` folder. The first step is starting the private testnet with `start_testnet_node.sh` script. The testnet is ready when the terminal displays `Testnet is ready for testing!`. The second step is running the python script `run_tests.py`. This will automatically run the test cases against the private testnet.