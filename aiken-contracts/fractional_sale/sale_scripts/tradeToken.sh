#!/bin/bash
set -e

source ../.env

# Addresses
sender_address=$(cat wallets/buyer-wallet/payment.addr)
receiver_address=$(cat wallets/seller-wallet/payment.addr)
# receiver_address="addr_test1qppp0mcjel3kztftz36k0cc5j77qcz4vt0udvc3fzzz9djeq7v27xnzv5664myxhzfvgj5tlymus096u0y7xfrx9sazqpvjzrv"

# Define Asset to be printed here
assetA="100000000 c34332d539bb554707a2d8826f2057bc628ac433a779c2f43d4a5b5c.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"
assetB="12345 6effa18e41008cd0b13f3959a5a4af40b92ca936bb7669f40d3b1f81.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6732"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address} + 5000000 + ${assetA}" | tr -dc '0-9')

# change_to_be_traded="${sender_address} + ${min_utxo} + ${assetB}"
# change_to_be_traded="${sender_address} + ${min_utxo} + ${assetA}"
# token_to_be_traded="${receiver_address} + 10000000000"
token_to_be_traded="${receiver_address} + ${min_utxo} + ${assetA}"

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
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${token_to_be_traded}" \
    ${network})

    # --tx-out="${change_to_be_traded}" \
    # --tx-out-inline-datum-file data/datum/attack_book_datum.json \
IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/buyer-wallet/payment.skey \
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