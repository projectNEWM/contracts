#!/bin/bash
set -e
source ../.env

if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Lovelace Amount To Extract'
    exit 1
fi

variable=${1}; jq --argjson variable $variable '.fields[0].fields[0].int=$variable' data/purchase_redeemer.json > data/purchase_redeemer.json-new.json
mv data/purchase_redeemer.json-new.json data/purchase_redeemer.json

# IF INPUT OF TOKENS

#
script_path="../marketplace-contract/marketplace-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

# collat, buyer, reference
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)

collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
# MATH OF REMAINING TOKENS

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
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

pid=$(cat market.json | jq -r .tokenPid)
tkn=$(cat market.json | jq -r .tokenTkn)

totalTknAmount=$(jq -r --arg policy_id "$pid" --arg name "$tkn" '[to_entries[] | select(.value.value[$policy_id][$name] >= 1) | .value.value[$policy_id][$name]] | add' tmp/script_utxo.json)
totalAdaAmount=$(jq -r --arg policy_id "$pid" --arg name "$tkn" '[to_entries[] | select(.value.value.lovelace >= 1) | .value.value.lovelace] | add' tmp/script_utxo.json)

echo "Total Coin:" ${totalTknAmount}
echo "Total Ada:" ${totalAdaAmount}

tknAmt=$((${totalTknAmount}))
MINT_ASSET="${tknAmt} ${pid}.${tkn}"

adaAmt=$((${totalAdaAmount} - ${1}))

script_address_out="${script_address} + ${adaAmt} + ${MINT_ASSET}"
buyer_address_out="${buyer_address} + ${1}"
echo "Mint OUTPUT: "${script_address_out}
echo "USER OUTPUT: "${buyer_address_out}
#
# exit
#
# Get tx payer info
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
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
# TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
TXIN=$(jq -r --arg alltxin "" --arg policy_id "" --arg name "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)

buyer_tx_in=${TXIN::-8}

# collat info
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
echo "collat utxo" $collat_utxo
# oracle info
echo -e "\033[0;36m Gathering Oracle UTxO Information  \033[0m"
oracle_path="../cogno-contract/cogno-contract.plutus"
oracle_address=$(${cli} address build --payment-script-file ${oracle_path} ${network} )
${cli} query utxo \
    ${network} \
    --address ${oracle_address} \
    --out-file tmp/oracle_utxo.json
TXNS=$(jq length tmp/oracle_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${rep_address} \033[0m \n";
   exit;
fi

oracle_id=$(cat market.json | jq -r .oraclePid)
oracle_name=$(cat market.json | jq -r .oracleTkn)
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$oracle_id" --arg token_name "$oracle_name" 'to_entries[] | select(.value.value[$policy_id][$token_name] >= 1) | .key | . + $alltxin + " --tx-in"' tmp/oracle_utxo.json)
oracle_tx_in=${TXIN::-8}
echo "oracle utxo" ${oracle_tx_in}

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#2" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/extract_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/marketplace_datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${buyer_pkh} \
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
