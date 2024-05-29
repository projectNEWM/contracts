# NEWM's Fractionalization Contract

A collection of smart contracts for Project NEWM.

Fractionalization refers to the process of dividing the ownership of streaming rights of a piece of art into 100 million pieces. The resulting fractions are given to the artist, who has the freedom to sell or trade them as they wish, or can be sold on the NEWM marketplace with the proceeds going directly to the artist.

## Updating Aiken

Please refer to the official documentation for [installation instructions](https://github.com/aiken-lang/aiken?tab=readme-ov-file#installation).

If `cargo` is already on path then this will install the lastest commit.

```
cargo install --git https://github.com/aiken-lang/aiken.git -f
```

Or just use the default updater to update aiken to the newest version.

```
aikup
```

### assist library

Project NEWM uses the Assist libray, a library of specialized Aiken functions for smart contracts on Cardano.

https://github.com/logicalmechanism/assist

Documentation for Assist can be view [here](https://www.logicalmechanism.io/docs/index.html).

## Build

Set up the `config.json` file with the correct starter token information, NEWM hot key, upper fee bounds, and pool id. These values are used inside the `complete_build.sh` script to compile and apply the contracts. The build script will auto generate the correct datums and redeemers.

All the tests can be ran with Aiken.

```
aiken check
```

## Happy Path Setup

The `scripts` folder assumes there will be test wallets inside the `wallets`folder.

```
artist-wallet
collat-wallet
reference-wallet
keeper1-wallet
keeper2-wallet
keeper3-wallet
newm-wallet
reward-wallet
starter-wallet
```

Use the `create_wallet.sh` script to auto generate simple wallets for testing.

First create the reference scripts with `00_createScriptReferences.sh` found in the scripts folder. This script will use funds held on the reference-wallet to store the smart contracts on UTxOs.

Next, we need to create the data reference UTxO using `01_createReferenceUTxO.sh` script inside the `reference` subfolder. This script will use the the starter-wallet to send the starter token defined in `config.json` into the data reference contract. The starter token acts like a pointer for the other contracts to correctly identify the true reference data. The `reference` folder contains test scripts for updating data on the data UTxO. These scripts require a valid n-out-of-m multisig with the set of keeper wallets held on a datum in the data reference contract. The `keeper*-wallet` do not need funds as they are just signing the transaction. The update scripts assume the `newm-wallet` will pay for the transaction fees.

After the reference data contract has been set up, register and delegate the stake key inside the `staking` subfolder. The staking contract will delegate the ada value inside the cip68 storage and sale contract to a specific pool defined in the `config.json` file. The test scripts in the `staking` folder assume that the `newm-wallet` will pay for the transaction fee.

At this point tokens may be minted and the sale contract can be used.

## Mint

Inside the `mint` subfolder are files for managing minting and burning tokens. A pair of tokens is minted each time the mint validator is executed, 1 reference token is sent to the cip68 storage contract and 100 million fractions are sent to the sale contract. The sale parameters are currently generated inside the `01_mintTokens.sh` script. Update the sale data by updating that script.

During minting, both tokens must exist inside the same tx but burning allows either just some amount of fractions, the reference, or both to be burned. A burning validation requires the valid multisig defined in the data reference contract.

## Sale

The sale contracts allows tokens to be sold at a fixed price. A potential buyer will pick some amount of tokens they wish to purchase. They place their purchase order into the queue contract that is then batched together with the sale contract to automate the sale.

