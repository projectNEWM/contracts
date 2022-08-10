#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../nft-locking-contract/nft-locking-contract.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
seller_address=$(cat wallets/seller-wallet/payment.addr)

sc_address_out="${SCRIPT_ADDRESS} + 5000000 + 1 982f93a0efde8edd0e9af400da083e91d98e1d5b4a77a07938a4de4f.74686973697361766572796c6f6e67737472696e67666f7274657374696e3130"
echo "Script OUTPUT: "${sc_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

# transaction variables
TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${sc_address_out}" \
    --tx-out-inline-datum-file data/current_datum.json  \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed