#!/bin/bash
set -e

# create directories if dont exist
mkdir -p contracts
mkdir -p hashes
mkdir -p certs

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
aiken build

echo -e "\033[1;33m Convert Reference Contract \033[0m"
aiken blueprint convert -v data_reference.data_reference > contracts/reference_contract.plutus
cardano-cli transaction policyid --script-file contracts/reference_contract.plutus > hashes/reference_contract.hash

# reference hash
ref=$(cat hashes/reference_contract.hash)

# the reference token
pid=$(jq -r '.starterPid' start_info.json)
tkn=$(jq -r '.starterTkn' start_info.json)

# cbor representation
ref_cbor=$(python ./convert_to_cbor.py ${ref})
pid_cbor=$(python ./convert_to_cbor.py ${pid})
tkn_cbor=$(python ./convert_to_cbor.py ${tkn})

# The pool to stake at
poolId=$(jq -r '.poolId' start_info.json)

echo -e "\033[1;33m Convert CIP68 Contract \033[0m"
aiken blueprint apply -o plutus.json -v cip68.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v cip68.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v cip68.params "${ref_cbor}" .
aiken blueprint convert -v cip68.params > contracts/cip68_contract.plutus

# build the stake contract
echo -e "\033[1;33m Convert Stake Contract \033[0m"
aiken blueprint apply -o plutus.json -v staking.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v staking.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v staking.params "${ref_cbor}" .
aiken blueprint convert -v staking.params > contracts/stake_contract.plutus
cardano-cli transaction policyid --script-file contracts/stake_contract.plutus > hashes/stake.hash
cardano-cli stake-address registration-certificate --stake-script-file contracts/stake_contract.plutus --out-file certs/stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file contracts/stake_contract.plutus --stake-pool-id ${poolId} --out-file certs/deleg.cert

echo -e "\033[1;33m Convert Sale Contract \033[0m"
aiken blueprint convert -v sale.sale > contracts/sale_contract.plutus

echo -e "\033[1;33m Convert Minting Contract \033[0m"
aiken blueprint apply -o plutus.json -v minter.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v minter.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v minter.params "${ref_cbor}" .
aiken blueprint convert -v minter.params > contracts/mint_contract.plutus
cardano-cli transaction policyid --script-file contracts/mint_contract.plutus > hashes/policy.hash

echo -e "\033[1;32m Building Complete! \033[0m"
