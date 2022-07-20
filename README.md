# Project Newm Contracts

A collection of contracts used my Project Newm.

# E2E Testing

The initial testing will assume Newm holds a special wallet that will regulate all token minting. In the future, this will be expanded into more decentralized solutions.

## Flow

An artist comes to Newm wishing to tokenize a song then fractionalize the tokenization. First, we need to create the initial tokenized version of the art by minting a token on the official Newm catalog policy id. The current implementation of the contract is designed such that many Newm catalogs of different types of art can coexist, each with a unique name prefix. An artist may keep the original tokenized version of their art or they may fractionalize the original token. 

The fractionalization of a token is the splitting of the ownership rights of the original tokenized art into 100 million pieces.