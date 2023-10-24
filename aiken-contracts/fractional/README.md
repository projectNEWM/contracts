# NEWM's Fraction Minting Contract

## Updating Aiken

This will install the lastest commit.

```
cargo install --git https://github.com/aiken-lang/aiken.git -f
```

Or just use the default updater.

```
aikup
```

## Build

Set up the `start_info.json` file with the correct starter token information and pool id. This will be used inside the `complete_build.sh` script to compile and apply the contract. The build script will auto generate the correct datum for the data reference contract and the staking contract.

All the tests can be ran with 

```
aiken check
```

## Setup

The `scripts` folder assumes there will be test wallets inside the wallets folder.

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

First create the reference scripts with `00_createScriptReferences.sh` found in the scripts folder. This script will use funds held on the reference-wallet to store the smart contracts on UTxOs. Currently, there are 5 contracts that require approximately 75 ADA to store.

Next, we need to create the data reference UTxO using `01_createReferenceUTxO.sh` script inside the `reference` subfolder. This script will use the the starter-wallet to send the starter token defined in `start_info.json` into the data reference contract. The starter token acts like a pointer for the other contracts to correctly identify the true reference data. The `reference` folder contains test scripts for updating data on the data UTxO. These scripts require a valid n-out-of-m multisig with the set of keeper wallets held on a datum in the data reference contract. The `keeper*-wallet` do not need funds as they are just signing the transaction. The update scripts assume the `newm-wallet` will pay for the transaction fees.

After the data contract has been set up, register and delegate the stake key. The staking contract will delegate the ada value inside the cip68 and sale contract to a specific pool. The test scripts in the `staking` folder assume that the `newm-wallet` will pay for the transaction fee. To withdraw rewards from the staking contract it requires a non-zero reward value.

At this point tokens may be minted and the sale contract can be used.

## Mint

Inside the `mint` folder are the files required to manage minting and burning tokens. A pair of tokens is minted each time the mint validator is executed. 1 reference token is sent to the cip68 contract and 100 million fractions are sent to the sale contract. The sale parameters are currently generated inside the `01_mintTokens.sh` script. Update the sale data by updating that script. The tokens do not have a validated destination so the reference and fractions can be sent anywhere.

During minting, both tokens must exist inside the same tx but burning allows either just some amount of fractions, the reference, or both to be burned. A burning validation requires the valid multisig defined in the data reference contract.

## Bundle Sale

A potential buyer will pick some amount of bundles they wish to purchase that is less than or equal to the defined maximum inside the sale datum. This max bundle size is arbitrary and can be changed to whatever is required for the sale. The seller has the ability to update or remove their sale at will.

The bundle token information needs to match what is on the UTxO and the cost token information can be whatever token the seller wishes to be paid in for a completed sale.

### Example Sale

A bundle sale with a bundle size of 100 tokens and a cost of 10 ADA. On the UTxO for the sale there will be

```bash
1000 policy.token_name
```

A buyer wants 6 bundles. They will receive 600 tokens at a cost of 60 ADA. The remaining 400 tokens will be sent back to the contract for another sale and the seller will receive 60 ADA.

```bash
# script return
400 policy.token_name
# buyer payout
600 policy.token_name
# seller payout
60 ADA
```

## Order Book 

- TODO

## Staking

The staking contract can only be registered and redelegated. It can not be deregistered. The only pool the stake script is allowed to redelegate to is the pool defined in the data contract. The stake credential will accumulate rewards over time and at any point may be rewarded the ADA to the reward address defined in the data contract. Because of the min utxo required, rewards below the minimum can not be taken out as the contract expects an exact payout of the reward.

## Data Referencing

The data reference contract contains all required information for the fractional contract system to work. It allows the system to dynamically evolve post compile without requiring hardforks.
