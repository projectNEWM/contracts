# bundle-sale

The bundle sale is designed for a user to place some large amount of a token onto a UTxO inside the contract for sale. A sale has a defined bundle size and cost. A potential buyer will pick some amount of bundles they want to purchase that is less than the contract defined maximum. 

The datum is set up to define the owner address, the bundle token information, and the cost token information.

```rust
pub type Datum {
  owner: OwnerInfo,
  bundle: TokenInfo,
  cost: TokenInfo,
}
```

The bundle token information needs to match what is on the UTxO and the cost token information can be whatever token the seller wishes to be paid in. The maximum bundle size is a hardcoded maximum of 10 bundles. This is an arbitrary pick and can be changed to whatever is needed in production.


## Building

All the tests can be ran with 

```
aiken check
```

and building the contract is as simple as

```
aiken build
aiken blueprint convert > fractional_sale.plutus
```

## Quick Example

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

## Happy Path Test Scripts

The test scripts inside the scripts folder follow the happy path for the contract. It will need wallets and test ada but should be enough code to test the contract in the simple happy path case. The scripts are designed to be used in sequential order.