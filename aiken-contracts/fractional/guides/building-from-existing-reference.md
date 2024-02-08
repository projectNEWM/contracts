# Building The Contracts From An Existing Deployment

The reference data, minter, and cip68 contract are live. This means that the reference data contract hash and the starter token are fixed and their current value must be used to parameterize the new contracts.

Update the `refDataHash`, `starterPid`, and `starterTkn` values in `start_info.json` with the current reference data contract hash and starter token information.

```bash
./initial_sale_build.sh
```

Using the existing contract and token information the `initial_sale_build.sh` will build out just the required contracts and print their contract hashes to the console. This information will be used in the reference data update.