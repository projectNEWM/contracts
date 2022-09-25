#!/bin/bash
set -e

if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A metadata.json file to represent the fractionalized tokens!'
    exit 1
fi

metadata_json_source=$1

source ../.env

#
script_path="../locking-contract/locking-contract.plutus"
mint_path="../minting-contract/minting-contract.plutus"
#
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})
#
deleg_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)
#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)
#
policy_id=$(cat ../minting-contract/policy.id)
nft_id=$(cat ../nft-minting-contract/policy.id)
#

token_name=$(cat ../start_info.json | jq -r .starterTkn)
token_number=$(cat ../tokenize-scripts/data/current_datum.json | jq -r .fields[1].int)

name=${token_name}$(echo -n "${token_number}" | xxd -ps)
name_ascii=$(echo -n "$name" | xxd -p -r)

sed -e "s/<policy_id_hex>/${policy_id}/g" -e "s/<asset_name_ascii>/${name_ascii}/g" ${metadata_json_source} | jq . > /tmp/metadata.json

SC_ASSET="1 ${nft_id}.${name}"
#
MINT_ASSET="100000000 ${policy_id}.${name}"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${buyer_address} + 5000000 + ${MINT_ASSET}" | tr -dc '0-9')

#
script_address_out="${script_address} + 18446744073709551615 + 1 b0818471a0e9633ae337cc1dcc7526ebe42286b4ceb3d836ad3a9e73.74686973697361766572796c6f6e67737472696e67666f7274657374696e6773"
fractional_nft_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum.json | tr -dc '0-9')


script_address_out="${script_address} + ${fractional_nft_min_utxo} + ${SC_ASSET}"
buyer_address_out="${buyer_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
echo "Script OUTPUT: "${script_address_out}
echo "Mint OUTPUT: "${buyer_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${buyer_address} \
    --out-file tmp/buyer_utxo.json

TXNS=$(jq length tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
# Gather only UTxO that are ada-only or are holding NFTs (not FTs)
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select((.value.value | length < 2) or .value.value[]?[]?==1) | .key | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
buyer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    ${network} \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
# Gather only the first UTxO that holds no native assets
script_tx_in=$(cat tmp/script_utxo.json | jq 'to_entries[] | select(.value.value | length < 2) | .key' | jq -rs '.[0]')

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

# exit
echo -e "\033[0;36m Building Tx \033[0m"

FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${buyer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/lock_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum.json \
    --required-signer-hash ${deleg_pkh} \
    --required-signer-hash ${collat_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/datum.json \
    --metadata-json-file /tmp/metadata.json \
    ${network})

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
    --signing-key-file wallets/delegator-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
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


echo -e "\033[0;35m THE TOKENIZED OBJECT HAS BEEN FRACTIONALIZED \033[0m"
