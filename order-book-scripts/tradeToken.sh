#!/bin/bash
set -e

source ../.env

# Addresses
sender_address=$(cat wallets/seller-wallet/payment.addr)
# receiver_address=$(cat wallets/buyer-wallet/payment.addr)
receiver_address="addr_test1qrupt9d9ug2ufnrrajp2q7gwvmrtzzgr80p5ug7q8nt4d66hu0s5mnhxh2853wtsgn9gdz6wuqtaqnkv0yk78p474d6qudapqh"

# Define Asset to be printed here
asset="1 df8e2ddd300b54a606611a8f20be1aef06dc38d560e00b73e57de5a1.0104a79b1ff28d52381de95b82feb2e56bec6d9533eb005b8d711353d815d4a9"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address} + 5000000 + ${asset}" | tr -dc '0-9')

change_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"
token_to_be_traded="${receiver_address} + 300000"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "\nChange:\n" ${change_to_be_traded}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
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

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${change_to_be_traded}" \
    ${network})

    # --tx-out="${token_to_be_traded}" \
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
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx.signed