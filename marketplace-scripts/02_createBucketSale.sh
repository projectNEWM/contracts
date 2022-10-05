#!/bin/bash
set -e

source ../.env

#
#
script_path="../marketplace-contract/marketplace-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

# collat, buyer, reference
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)


token_pid=$(cat market.json | jq -r .tokenPid)
token_tkn=$(cat market.json | jq -r .tokenTkn)

echo -e "\033[0;36m Gathering Payer UTxO Information  \033[0m"
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

totalTknAmount=$(jq -r --arg policy_id "$token_pid" --arg name "$token_tkn" '[to_entries[] | select(.value.value[$policy_id][$name] >= 1) | .value.value[$policy_id][$name]] | add' tmp/buyer_utxo.json)

MINT_ASSET="${totalTknAmount} ${token_pid}.${token_tkn}"

script_address_out="${script_address} + 2000000 + ${MINT_ASSET}"
echo "Mint OUTPUT: "${script_address_out}
#
# exit
#
# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/marketplace_datum.json  \
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
