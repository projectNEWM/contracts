#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat ./data/path_to_socket.sh)
cli=$(cat ./data/path_to_cli.sh)
testnet_magic=$(cat ./data/testnet.magic)

mkdir -p ./tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# contract path
mint_script_path="../fraction_minter.plutus"

# Addresses
reference_address=$(cat ./wallets/reference-wallet/payment.addr)
script_reference_address=$(cat ./wallets/reference-wallet/payment.addr)

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${mint_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

mint_value=$((${mint_min_utxo}))
mint_script_reference_utxo="${script_reference_address} + ${mint_value}"

echo -e "Creating Bank Script:\n" ${mint_script_reference_utxo}

#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${reference_address} \
    --out-file ./tmp/reference_utxo.json

TXNS=$(jq length ./tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ./tmp/reference_utxo.json)
ref_tx_in=${TXIN::-8}
#
# exit
#
# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' ./tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee 900000

FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
firstReturn=$((${starting_reference_lovelace} - ${mint_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}


#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-1.signed

#

cp ./tmp/tx-1.signed ./tmp/mint-reference-utxo.signed

