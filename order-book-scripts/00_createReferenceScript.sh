#!/bin/bash
set -e

source ../.env

order_book_script_path="../order-book-contract/order-book-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)

order_book_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${order_book_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "Order Book Min Fee" ${order_book_min_utxo}

echo
order_book_value=$order_book_min_utxo
order_book_script_reference_utxo="${reference_address} + ${order_book_value}"

echo -e "\nCreating NFT Minting Reference:\n" ${order_book_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${reference_address} \
    --out-file tmp/reference_utxo.json

TXNS=$(jq length tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/reference_utxo.json)
reference_tx_in=${TXIN::-8}

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"
starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${reference_tx_in} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${order_book_script_reference_utxo}" \
    --tx-out-reference-script-file ${order_book_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft ${network} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
fee=$(echo $FEE | rev | cut -c 9- | rev)
echo $fee

firstReturn=$((${starting_reference_lovelace} - ${order_book_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${reference_tx_in} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${order_book_script_reference_utxo}" \
    --tx-out-reference-script-file ${order_book_script_path} \
    --fee ${fee}
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx-1.signed

#
cp tmp/tx-1.signed tmp/tx-reference-utxo.signed