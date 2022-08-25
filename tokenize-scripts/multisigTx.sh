#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

# Addresses
sender_address=$(cat wallets/multisig-wallet/payment.addr)
receiver_address=$(cat wallets/buyer-wallet/payment.addr)
# receiver_address="addr_test1qrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdt2s7gj5l4pam3pdeckkp7jwx8dsxelvq3ypv2ggzet9wcsxrp7pu"

# Define Asset to be printed here
asset="1 49d5d9a180b652ef4163ecfd53ea1521d9794a44933848da9c1b65fb.6173757065726c6f6e676e616d6568657265776974686d61786c656e677432"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address} ${asset}" | tr -dc '0-9')

# token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"
token_to_be_traded="${receiver_address} + 1000000"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
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
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-in-script-file wallets/multisig-wallet/payment.script \
    --witness-override 3 \
    --tx-out="${token_to_be_traded}" \
    --required-signer-hash c9c81aeb38d02aa34b70e42a90e88435a729ea62d24b1eaeedf68e09 \
    --required-signer-hash a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439 \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#

# ${cli} transaction witness \
#     --tx-body-file tmp/tx.draft \
#     --signing-key-file wallets/buyer-wallet/payment.skey \
#     --out-file tmp/addr1.witness \
#     --testnet-magic ${testnet_magic}

# ${cli} transaction witness \
#     --tx-body-file tmp/tx.draft \
#     --signing-key-file wallets/seller-wallet/payment.skey \
#     --out-file tmp/addr2.witness \
#     --testnet-magic ${testnet_magic}

# ${cli} transaction assemble \
#     --tx-body-file tmp/tx.draft \
#     --witness-file tmp/addr1.witness \
#     --witness-file tmp/addr2.witness \
#     --out-file tmp/tx.signed

    # --signing-key-file wallets/buyer-wallet/payment.skey \
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
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