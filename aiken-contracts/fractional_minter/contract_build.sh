#!/bin/bash
set -e

# the pkh of the new hot key without the network tag
NEWM_KEY="E6F85717B932788E3B1E57B11E9F8BF190D257C57D369979C18A02DD"
# echo $NEW_key
# get cbor with the python one liner
NEWM_KEY_CBOR=$(python ./convert_to_cbor.py ${NEWM_KEY})
echo $NEWM_KEY_CBOR
echo "(con data #${NEWM_KEY_CBOR})"
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

# start with data reference
echo -e "\033[1;33m Convert Sale Contract \033[0m"
aiken blueprint convert -v fractional_sale.fractional_sale > fractional_sale.plutus


# minting contract policy id
cardano-cli transaction policyid --script-file fraction_minter.plutus > policy.id
echo -e "\033[1;36m Minting Policy Id: $(cat policy.id) \033[0m"