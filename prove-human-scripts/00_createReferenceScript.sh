#!/bin/bash
set -e

source ../.env

prove_human_script_path="../prove-human-contract/prove-human-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)

prove_human_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${prove_human_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "Fractional Sale Min Fee" ${prove_human_min_utxo}

echo
prove_human_value=$prove_human_min_utxo
prove_human_script_reference_utxo="${reference_address} + ${prove_human_value}"

echo -e "\nCreating NFT Minting Reference:\n" ${prove_human_script_reference_utxo}
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
    --tx-out="${prove_human_script_reference_utxo}" \
    --tx-out-reference-script-file ${prove_human_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft ${network} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
fee=$(echo $FEE | rev | cut -c 9- | rev)
echo -e "\033[1;32m Fee: \033[0m" $fee

firstReturn=$((${starting_reference_lovelace} - ${prove_human_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${reference_tx_in} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${prove_human_script_reference_utxo}" \
    --tx-out-reference-script-file ${prove_human_script_path} \
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