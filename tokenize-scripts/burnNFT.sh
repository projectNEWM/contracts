#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../v2-nft-locking-contract/v2-tokenized-locking-contract.plutus"
mint_path="../v2-nft-minting-contract/v2-tokenized-minting-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
#
policy_id=$(cat ../v2-nft-minting-contract/policy.id)
#
name=$(echo -n "NewM_0" | xxd -ps)
MINT_ASSET="-1 ${policy_id}.${name}"
# UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
#     --protocol-params-file tmp/protocol.json \
#     --tx-out="${seller_address} ${MINT_ASSET}" | tr -dc '0-9')
#
script_address_out="${script_address} + 5000000"
# seller_address_out="${seller_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
echo "Script OUTPUT: "${script_address_out}
# echo "Mint OUTPUT: "${seller_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${seller_address} \
    --out-file tmp/buyer_utxo.json

TXNS=$(jq length tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/buyer_utxo.json)
collateral_tx_in=${CTXIN::-19}
buyer_tx_in=${TXIN::-8}

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

# utxos
collat_utxo=$(cardano-cli transaction txid --tx-file tmp/tx.signed)
# collat_utxo="2b44b4ec9cd6fdd44211f49e02c7e1667c5545a7be65b6800bbec6a46ba8b919"
script_ref_utxo=$(cardano-cli transaction txid --tx-file tmp/tx-reference-utxo.signed)
voting_ref_utxo="cc9599064979336d5ab6b7759c4f8bf1677da632c04336962c46e0fbe0d054d5"


# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in ${buyer_tx_in} \
    --read-only-tx-in-reference="${voting_ref_utxo}#1" \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/burn_redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/next_datum.json \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/next_datum.json \
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