#!/bin/bash
set -e

NEWM_KEY="e6f85717b932788e3b1e57b11e9f8bf190d257c57d369979c18a02dd"
NEWM_KEY_CBOR=$(python ./convert_to_cbor.py ${NEWM_KEY})

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
aiken build

# start with data reference
echo -e "\033[1;33m Convert Minting Contract \033[0m"
aiken blueprint apply -v fractional_minter.fractional_minter . "(con data #${NEWM_KEY_CBOR})"
aiken blueprint convert -v fractional_minter.fractional_minter > fraction_minter.plutus

echo -e "\033[1;33m Convert CIP68 Contract \033[0m"
aiken blueprint apply -v cip68.cip68 . "(con data #${NEWM_KEY_CBOR})"
aiken blueprint convert -v cip68.cip68 > cip68.plutus

# minting contract policy id
cardano-cli transaction policyid --script-file fraction_minter.plutus > policy.id
echo -e "\033[1;36m Minting Policy Id: $(cat policy.id) \033[0m"