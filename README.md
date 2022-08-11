# Project Newm Contracts

A collection of contracts used my Project Newm.

- NEEDS UPDATE FOR MVP

# E2E Testing

The initial testing will assume Newm holds a special wallet that will regulate all token minting. In the future, this will be expanded into more decentralized solutions.

The example in the repo is with test wallets. A recompile with proper wallets will need to occur during the initial production phase.

## Flow

An artist comes to Newm wishing to tokenize an piece of art then fractionalize the tokenization. First, Newm will create the initial tokenized version of the art by minting a token on the official Newm catalog policy id. An artist may keep the original tokenized version of their art or they may fractionalize the original token.

The fractionalization of a token is the splitting of the ownership rights of the original tokenized art into 100 million pieces. The original tokenized art is locked into a smart contract and the resulting fractions are given to the artist. From here the artist my do what they wish with the fractions.

This entire process is reversible. At any point, an artist my obtain the original tokenized by burning the 100 million fractional tokens.

## Tokenization

Each UTxO inside the tokenization contract is a unique Newm catalog, completely determined by the token prefix name like "Newm_", "paintings_", or "songs_". The contract behaves as a sequential NFT generator, minting tokens using the concatentation of the prefix with the current token counter. The sequence of NFTs will continue forever or at least until the size of the token name exceeds 32 characters.

### Process

- Create catalog UTxO inside the tokenization contract.
- Mint sequential NFT for artist.
- (Optional) If required, any catalog NFT may be burned.
- (Optional) If required, any catalog may be removed.

** Each prefix and thus each UTxO must be unique.

### Example

```bash
./createCatalogUTxO.sh
./MintNFT.sh

# if required
./burnNFT.sh
./removedCatalogUTxO.sh
```
## Fractionalization

Each fractionalization of a token must be prepped by Newm or the artist. This ensures that only one token may be added onto an existing UTxO for fractionalization. There is no way to do entry validation for smart contracts on Cardano. Validation only occurs during a spending event thus forcing the artist or Newm to update an already existing UTxO means forcing on-chain validation. The fractionalized tokens will have the fractional Newm policy id but will share the same name as the original token. The token to be fractionalize may only be unlocked if and only if all 100 million fractions are burned together at once.

### Process

- Create fractional UTxO.
- Lock and mint token fractions.
- (Optional) If required, fractional tokens may be burned to unlock the original token.
- (Optional) If required, the left over fractional UTxO may be removed after successful burning.

** Fractionalization of tokens if the same names will result in two sets of fractions.

### Example

```bash
./createFractionalUTxO.sh
./lockAndFractionalize.sh

# if required
./unlockAndSolidify.sh
./removeFractionalUTxO.sh
```