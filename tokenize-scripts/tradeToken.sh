#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)


# Addresses
sender_address=$(cat wallets/reference-wallet/payment.addr)
receiver_address=$(cat wallets/seller-wallet/payment.addr)
# receiver_address="addr_test1qrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdt2s7gj5l4pam3pdeckkp7jwx8dsxelvq3ypv2ggzet9wcsxrp7pu"

# Define Asset to be printed here
asset="5500 5c58aad408cfe2d5221617a4941d2b352aa9b35491315c8d68519417.4f6e65566572794c6f6e67537472696e67466f7254657374436f6e7472616374"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-hash-value 42 \
    --tx-out="${receiver_address} ${asset}" | tr -dc '0-9')
change_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"
token_to_be_traded="${receiver_address} + 15000000"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "\nChange:\n" ${change_to_be_traded}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
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
    --change-address ${receiver_address} \
    --tx-in ${HEXTXIN} \
    --testnet-magic ${testnet_magic})

    # --tx-out="${token_to_be_traded}" \
    # --tx-out="${change_to_be_traded}" \
    # --tx-out="${change_to_be_traded}" \
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
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed