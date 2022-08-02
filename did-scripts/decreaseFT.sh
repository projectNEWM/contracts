#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../v2-did-locking-contract/v2-did-locking-contract.plutus"
mint_path="../v2-did-minting-contract/v2-did-minting-contract.plutus"


script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
#
policy_id=$(cat ../v2-did-minting-contract/policy.id)
#
name=$(echo -n "iou" | xxd -ps)
SC_ASSET="1000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"

MINT_ASSET="-1000000 ${policy_id}.${name}"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${seller_address} ${SC_ASSET}" | tr -dc '0-9')
#
script_address_out="${script_address} + 3000000"
seller_address_out="${seller_address} + ${UTXO_VALUE} + ${SC_ASSET}"
echo "Script OUTPUT: "${script_address_out}
echo "Mint OUTPUT: "${seller_address_out}
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

collat_utxo="732e3fca26b3cdd59ee25c96c93a71cd2736dd8279760cf71119ed67744cc9e6"
script_ref_utxo=$(cardano-cli transaction txid --tx-file tmp/tx-reference-utxo.signed)
voting_ref_utxo=$(cardano-cli transaction txid --tx-file ../voting-scripts/tmp/vote-tx.signed)

# voting_ref_utxo="40ad59b9786cfa827cc9a2dd3b174be3df0b31d60a931818f5495660bb061329"

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
    --spending-reference-tx-in-redeemer-file data/decrease_redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum.json \
    --required-signer-hash ${seller_pkh} \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --mint="${MINT_ASSET}" \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/datum.json \
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