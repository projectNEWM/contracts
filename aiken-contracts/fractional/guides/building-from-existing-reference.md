# Building Contracts from an Existing Deployment

This guide covers the process of building contracts using the details from an already deployed set of contracts. It specifically involves updating parameters with the current reference data and starter token values. Follow the steps below to update your contracts correctly.

## Prerequisites

Ensure you have the following:
- The reference data contract, minter, and cip68 contract are already deployed and live.
- Access to the current reference data contract hash and starter token.

## Updating Contract Parameters

1. **Locate the `start_info.json` File**: This file contains parameters that need to be updated with live contract data.

2. **Edit `start_info.json`**: Update the following values in the `start_info.json` file with the current data:
    - `refDataHash`: The hash of the live reference data contract.
    - `starterPid`: The project ID associated with the starter token.
    - `starterTkn`: Information about the starter token.

   These updates ensure that the new contracts are correctly parameterized with the existing deployment's data.

## Building the Contracts

Once the necessary parameters are updated in `start_info.json`, proceed with building the contracts:

```bash
./initial_sale_build.sh
```

Executing the `initial_sale_build.sh` script will:
- Utilize the existing contract and token information.
- Build only the required contracts.
- Print the new contract hashes to the console for further use.

## Reference Data Update

- Use the contract hashes output from the script as part of the reference data update process.

By following these steps, you will successfully update and build your contracts based on an existing deployment.
