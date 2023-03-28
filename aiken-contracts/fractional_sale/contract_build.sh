#!/bin/bash
set -e

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
aiken build

# start with data reference
echo -e "\033[1;33m Convert Contract \033[0m"
aiken blueprint convert > fractional_sale.plutus
