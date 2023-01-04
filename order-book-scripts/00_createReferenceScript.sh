#!/bin/bash
set -e

source ../.env

cogno_script_path="../cogno-contract/cogno-contract.plutus"
market_script_path="../marketplace-contract/marketplace-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${cogno_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "NFT Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${market_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "NFT Minting Min Fee" ${mint_min_utxo}

echo
market_value=$mint_min_utxo
cogno_value=$lock_min_utxo
cogno_script_reference_utxo="${reference_address} + ${cogno_value}"
market_script_reference_utxo="${reference_address} + ${market_value}"

echo -e "\nCreating NFT Locking Reference:\n" ${cogno_script_reference_utxo}
echo -e "\nCreating NFT Minting Reference:\n" ${market_script_reference_utxo}
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
HEXTXIN=${TXIN::-8}
# echo $HEXTXIN
# exit

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${cogno_script_reference_utxo}" \
    --tx-out-reference-script-file ${cogno_script_path} \
    --tx-out="${market_script_reference_utxo}" \
    --tx-out-reference-script-file ${market_script_path} \
    --fee 900000
FEE=$(${cli} transaction calculate-min-fee --tx-body-file tmp/tx.draft ${network} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
firstReturn=$((${starting_reference_lovelace} - ${market_value} - ${cogno_value} - ${fee}))
# echo $firstReturn
# exit
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${cogno_script_reference_utxo}" \
    --tx-out-reference-script-file ${cogno_script_path} \
    --tx-out="${market_script_reference_utxo}" \
    --tx-out-reference-script-file ${market_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    ${network}
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx-1.signed

cp tmp/tx-1.signed tmp/tx-reference-utxo.signed