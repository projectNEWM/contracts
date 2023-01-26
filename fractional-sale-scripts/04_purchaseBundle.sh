#!/bin/bash
set -e
source ../.env

#
script_path="../fractional-sale-contract/fractional-sale-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

# collat, seller, reference
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)

#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)

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

TXIN=$(jq -r --arg alltxin "" --arg sellerPkh "${seller_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $sellerPkh) | .key | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
seller_utxo=${TXIN::-8}
echo SELLER UTXO ${seller_utxo}

pid="0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52"
tkn="5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"
CURRENT_VALUE=$(jq -r --arg sellerPkh "${seller_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $sellerPkh) | .value.value[$pid][$tkn]' tmp/script_utxo.json)
echo $VALUE

if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Supply A Bundle Amount \033[0m \n";
    exit
fi
if [[ ${1} -eq 0 ]] ; then
    echo -e "\n \033[0;31m Bundle Size Must Be Greater Than Zero \033[0m \n";
    exit
fi
if [[ ${1} -gt 10 ]] ; then
    echo -e "\n \033[0;31m Bundle Size Must Be Less Than Or Equal To Ten \033[0m \n";
    exit
fi

# # update the starting lock time
# variable=${startTime}; jq --argjson variable "$variable" '.fields[2].int=$variable' data/datum/stake-datum.json > data/datum/stake-datum-new.json
# mv data/datum/stake-datum-new.json data/datum/stake-datum.json


exit



buyer_asset="1000000000000 0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

buyer_utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address} + 5000000 + ${buyer_asset}" | tr -dc '0-9')

seller_asset="100000000000000 0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

script_utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file data/datum/sale_datum.json \
    --tx-out="${script_address} + 5000000 + ${seller_asset}" | tr -dc '0-9')

returning_asset="99000000000000 0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"


script_address_out="${script_address} + ${script_utxo_value} + ${returning_asset}"
buyer_address_out="${buyer_address} + ${buyer_utxo_value} + ${buyer_asset}"
seller_address_out="${seller_address} + 100000000"
echo "Script OUTPUT: "${script_address_out}
echo "Buyer OUTPUT: "${buyer_address_out}
echo "Seller OUTPUT: "${seller_address_out}
#
# exit
#
# Get tx payer info
echo -e "\033[0;36m Gathering Batcher Bot UTxO Information  \033[0m"
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
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
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

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${buyer_tx_in} \
    --tx-in ${seller_utxo} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/redeemer/purchase_redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum/sale_datum.json  \
    --required-signer-hash ${collat_pkh} \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/profit-wallet/payment.skey \
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
