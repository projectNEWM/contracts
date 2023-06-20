#!/bin/bash
set -e

function cat_file_or_empty() {
  if [ -e "$1" ]; then
    cat "$1"
  else
    echo ""
  fi
}


# create directories if dont exist
mkdir -p contracts
mkdir -p hashes

# remove old files
rm contracts/* || true
rm hashes/* || true

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
# aiken build
aiken build --keep-traces

echo -e "\033[1;33m Convert Sale Contract \033[0m"
aiken blueprint convert -v order_book.params > contracts/order_book_contract.plutus
cardano-cli transaction policyid --script-file contracts/order_book_contract.plutus > hashes/order_book.hash

# end of build
echo -e "\033[1;32m Building Complete! \033[0m"