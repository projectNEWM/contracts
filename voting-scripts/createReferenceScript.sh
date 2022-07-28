#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

TESTNET_MAGIC=1097911063

vote_script_path="../v2-voting-contract/v2-voting-contract.plutus"

# Addresses
sender_address=$(cat wallets/seller-wallet/payment.addr)
receiver_address=$(cat wallets/reference-wallet/payment.addr)

vote_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${vote_script_path} \
    --tx-out="${receiver_address} 0" | tr -dc '0-9')
echo "Voting Min Fee" ${vote_min_utxo}

vote_script_reference_utxo="${receiver_address} + 19640670"

echo -e "\nCreating Voting Reference:\n" ${vote_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${TESTNET_MAGIC} \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}
echo $HEXTXIN
# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${vote_script_reference_utxo}" \
    --tx-out-reference-script-file ${vote_script_path} \
    --testnet-magic ${TESTNET_MAGIC})

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
    --out-file tmp/tx-reference-utxo.signed \
    --testnet-magic ${TESTNET_MAGIC}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file tmp/tx-reference-utxo.signed