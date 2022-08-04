#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../v2-voting-contract/v2-voting-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
seller_address=$(cat wallets/seller-wallet/payment.addr)

SC_ASSET="30000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${seller_address} ${SC_ASSET}" | tr -dc '0-9')


script_address_out="${script_address} + 5000000 + 1 982f93a0efde8edd0e9af400da083e91d98e1d5b4a77a07938a4de4f.74686973697361766572796c6f6e67737472696e67666f7274657374696e3130"
seller_address_out="${seller_address} + ${UTXO_VALUE} + ${SC_ASSET}"
echo "Vote Script OUTPUT: "${script_address_out}
echo "Vote Self OUTPUT: "${seller_address_out}

#
# exit
#

echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/seller_utxo.json)
collateral_tx_in=${CTXIN::-19}
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(cardano-cli transaction txid --tx-file tmp/tx-reference-utxo.signed)
collat_utxo="cda1278d68009926f59e67ed8e950fdd43ab4e52069ff22bfe240c9b503981a1"


echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/vote_redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/next_datum.json  \
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
    --out-file tmp/vote-tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/vote-tx.signed