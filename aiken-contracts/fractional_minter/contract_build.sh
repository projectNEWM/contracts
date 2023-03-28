#!/bin/bash
set -e

rm fraction_minter.plutus

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
aiken build

# start with data reference
echo -e "\033[1;33m Convert Contract \033[0m"
aiken blueprint convert > fraction_minter.plutus

# minting contract policy id
cardano-cli transaction policyid --script-file fraction_minter.plutus > policy.id
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"