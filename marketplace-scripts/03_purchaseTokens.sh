#!/bin/bash
set -e
source ../.env

if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Lovelace Amount To Spend'
    exit 1
fi

variable=${1}; jq --argjson variable $variable '.fields[0].fields[0].int=$variable' data/purchase_redeemer.json > data/purchase_redeemer.json-new.json
mv data/purchase_redeemer.json-new.json data/purchase_redeemer.json

# IF INPUT OF TOKENS

#
script_path="../marketplace-contract/marketplace-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

# collat, seller, reference
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

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

pid="3f1eb5125fbde17a5a5cf96be0b9863142a34f677bf84cef63c699af"
tkn="537461626c65"

totalTknAmount=$(jq -r --arg policy_id "$pid" --arg name "$tkn" '[to_entries[] | select(.value.value[$policy_id][$name] >= 1) | .value.value[$policy_id][$name]] | add' tmp/script_utxo.json)
totalAdaAmount=$(jq -r --arg policy_id "$pid" --arg name "$tkn" '[to_entries[] | select(.value.value.lovelace >= 1) | .value.value.lovelace] | add' tmp/script_utxo.json)

echo "Total Coin:" ${totalTknAmount}
echo "Total Ada:" ${totalAdaAmount}

adaPrice=$(cat data/cogno_datum.json | jq -r .fields[0].fields[4].int)
tknPrice=$(cat data/cogno_datum.json | jq -r .fields[0].fields[7].int)
wantedTknAmt=$(python3 -c "print((${1} * ${tknPrice}) // (${adaPrice}))")

echo "Received Coin: "${wantedTknAmt}

if [ $((${wantedTknAmt} <= ${totalTknAmount})) -eq "0" ]; then
   echo "Not Enough Coin";
   exit 1
fi

tknAmt=$((${totalTknAmount} - ${wantedTknAmt}))
MINT_ASSET="${tknAmt} ${pid}.${tkn}"

adaAmt=$((${totalAdaAmount} + ${1}))

REC_ASSET="${wantedTknAmt} ${pid}.${tkn}"

script_address_out="${script_address} + ${adaAmt} + ${MINT_ASSET}"
seller_address_out="${seller_address} + 2000000 + ${REC_ASSET}"
echo "Mint OUTPUT: "${script_address_out}
echo "USER OUTPUT: "${seller_address_out}
#
# exit
#
# Get tx payer info
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
# TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
TXIN=$(jq -r --arg alltxin "" --arg policy_id "" --arg name "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)

seller_tx_in=${TXIN::-8}

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

oracle_id="b99c2b76eaa57f90aaf0a6c61e52210696e6662f483693e316747e1a"
oracle_name="737461727465725f6e6674"
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
    --change-address ${seller_address} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${seller_tx_in} \
    --read-only-tx-in-reference ${oracle_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#2" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/purchase_redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/marketplace_datum.json \
    --required-signer-hash ${collat_pkh} \
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
    --signing-key-file wallets/seller-wallet/payment.skey \
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
