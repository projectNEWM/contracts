#!/bin/bash
set -e

source ../.env

lock_script_path="../locking-contract/locking-contract.plutus"
mint_script_path="../minting-contract/minting-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)

# lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file tmp/protocol.json \
#     --tx-out-reference-script-file ${lock_script_path} \
#     --tx-out="${reference_address} 0" | tr -dc '0-9')
# echo "Locking Min Fee" ${lock_min_utxo}

# mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file tmp/protocol.json \
#     --tx-out-reference-script-file ${mint_script_path} \
#     --tx-out="${reference_address} 0" | tr -dc '0-9')
# echo "Minting Min Fee" ${mint_min_utxo}
# lock_value=$((${lock_min_utxo} + 1000000))
# mint_value=$((${mint_min_utxo} + 1000000))

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${mint_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "Minting Min Fee" ${mint_min_utxo}

mint_value=$mint_min_utxo
lock_value=$lock_min_utxo

lock_script_reference_utxo="${reference_address} + ${lock_value}"
mint_script_reference_utxo="${reference_address} + ${mint_value}"

echo -e "\nCreating Locking Reference:\n" ${lock_script_reference_utxo}
echo -e "\nCreating Minting Reference:\n" ${mint_script_reference_utxo}
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
echo $HEXTXIN
# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${reference_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-reference-utxo.signed \
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx-reference-utxo.signed