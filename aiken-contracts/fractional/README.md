# fractional

# Build

Set up the `start_info.json` file with the correct starter token information and pool id. This will be used inside the `contract_build.sh` script to compile and apply the contract. The build script will auto generate the correct datum for the data reference contract and the staking contract.

All the tests can be ran with 

```
aiken check
```

## Setup

The scripts folder assumes there will be test wallets inside the wallets folder.

```
artist-wallet
collat-wallet
keeper1-wallet
keeper2-wallet
keeper3-wallet
newm-wallet
reward-wallet
starter-wallet
```

Use the `create_wallet.sh` script to auto generate simple wallets for testing.

First create the reference scripts with `00_createScriptReferences.sh` found in the scripts folder.

Next, we need to create the data reference UTxO using `01_createReferenceUTxO.sh` script inside the reference subfolder. This folder contains test scripts for updating data on the data UTxO. These scripts require a valid multisig using the keeper set of test wallets.

After the data contract has been set up activate and delegate the stake key used to stake the ada value inside the cip68 and sale contract. Withdrawing rewards requires an reward value.

At this point tokens may be minted and the sale contract can be used.

### Mint

Inside the mint folder is all the files required to manage owned tokens. A pair of tokens are minted each time the validator is executed. 1 reference token sent to the cip68 contract and 100 M fractions sent to the sale contract. The sale parameters are generated inside the `01_mintTokens.sh` script. Currently, tokens do not have a validated destination so the reference and fractions can be sent anywhere.

During minting, both tokens must exist inside the same tx but burning allowing either just some amount of fractions or the reference to be burned. A burning validation requires a valid multisig.

### Sale

cost. A potential buyer will pick some amount of bundles they want to purchase that is less than the contract defined maximum. 

The datum is set up to define the owner address, the bundle token information, and the cost token information.

```rust
pub type Datum {
  owner: OwnerInfo,
  bundle: TokenInfo,
  cost: TokenInfo,
}
```

The bundle token information needs to match what is on the UTxO and the cost token information can be whatever token the seller wishes to be paid in. The maximum bundle size is a hardcoded maximum of 10 bundles. This is an arbitrary pick and can be changed to whatever is needed in production.

A bundle sale with a bundle size of 100 and a cost of 10 ADA.

```bash
1000 policy.token_name
```

A buyer wants 6 bundles. They will receive 600 tokens at a cost of 60 ADA.

```bash
# script return
400 policy.token_name
# buyer pay
600 policy.token_name
# seller pay
60 ADA
```