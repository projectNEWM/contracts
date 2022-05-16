# Project Newm Contracts

A user will lock a NFT into a contract and then mint the cooresponding fractions for that NFT.

# Compile Instructions

Compile LockingContract.hs

```bash
cardano-cli transaction policyid --script-file locking_contract.plutus > validator.hash
```

Place the validator hash of the plutus script into the minting policy.

Compile MintingContract.hs

```bash
cardano-cli transaction policyid --script-file minting_contract.plutus > policy.id
```