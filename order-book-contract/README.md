# NEWM Order Book Marketplace

The contract for the order book marketplace for NEWM on Cardano, written in Plutus V2.

## Order Creation

## Order Cancellation

## Order Updating

## Order Matching

### Full Swap

### Partial Swap

## Build Instructions

The cabal.project file may need to be updated for new dependencies due to on going updates.

```bash
cabal build -w ghc-8.10.7
cabal run order-book-contract
```
Or run the build-project shell script.

```bash
./build-project.sh
```
