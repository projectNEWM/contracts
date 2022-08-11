#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
#
mint_path="policy/policy.script"
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
collat_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)

policy_id=$(cat policy/policy.id)
# It'sTheStarterToken4ProjectNewM
TOKEN_NAME=$(cat ../start_info.json | jq -r .starterTkn)
MINT_ASSET="-1 ${policy_id}.${TOKEN_NAME}"

echo ""
echo -e "\033[0;31m THIS WILL BE REMOVED IN PRODUCTION  \033[0m"

#
# UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
#     --protocol-params-file tmp/protocol.json \
#     --tx-out="${seller_address} ${MINT_ASSET}" | tr -dc '0-9')

# seller_address_out="${seller_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
# echo "Mint OUTPUT: "${seller_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
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
seller_tx_in=${TXIN::-8}

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
    --mint-script-file policy/policy.script \
    --mint="${MINT_ASSET}" \
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
    --signing-key-file wallets/collat-wallet/payment.skey \
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

# update start_info.json