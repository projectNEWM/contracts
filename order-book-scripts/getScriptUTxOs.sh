#!/bin/bash
set -e
source ../.env

#
script_path="../order-book-contract/order-book-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    ${network} \
    --out-file py/script_utxos.json
