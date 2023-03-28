#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ./data/path_to_socket.sh)
cli=$(cat ./data/path_to_cli.sh)
testnet_magic=$(cat ./data/testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# cip 68 contract
cip68_script_path="../cip68.plutus"
cip68_script_address=$(${cli} address build --payment-script-file ${cip68_script_path} --testnet-magic ${testnet_magic})

# bundle sale contract
sale_script_path="../../fractional_sale/fractional_sale.plutus"
sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --testnet-magic ${testnet_magic})

#
newm_address=$(cat wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/newm-wallet/payment.vkey)

# for testing
bad_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/artist-wallet/payment.vkey)

#
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
#
receiver_address=$(cat wallets/artist-wallet/payment.addr)

#
policy_id=$(cat ../policy.id)

echo -e "\033[0;36m Gathering NEWM UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${newm_address} \
    --out-file tmp/newm_utxo.json

TXNS=$(jq length tmp/newm_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${newm_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/newm_utxo.json)
newm_tx_in=${TXIN::-8}

echo "NEWM UTxO:" $newm_tx_in

string=${newm_tx_in}
IFS='#' read -ra array <<< "$string"

prefix_100="2831303029"
prefix_333="2833333329"

# for testing
prefix_bad="283329"

ref_name=$(python3 -c "import sys; sys.path.append('../lib/py/'); from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_100}')")
frac_name=$(python3 -c "import sys; sys.path.append('../lib/py/'); from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_333}')")

# update bundle sale datum with frac token name
bundle_size=10000000
lovelace_price=1000000
jq \
--arg policy_id "$policy_id" \
--arg frac_name "$frac_name" \
--argjson bundle_size "$bundle_size" \
--argjson lovelace_price "$lovelace_price" \
'.fields[1].fields[0].bytes=$policy_id | 
.fields[1].fields[1].bytes=$frac_name |
.fields[1].fields[2].int=$bundle_size |
.fields[2].fields[2].int=$lovelace_price 
' \
./data/sale-datum.json | sponge ./data/sale-datum.json
cp ./data/sale-datum.json ../../fractional_sale/scripts/data/datum/sale_datum.json


REFERENCE_ASSET="1 ${policy_id}.${ref_name}"
FRACTION_ASSET="100000000 ${policy_id}.${frac_name}"

MINT_ASSET="1 ${policy_id}.${ref_name} + 100000000 ${policy_id}.${frac_name}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/metadata-datum.json \
    --tx-out="${cip68_script_address} + 5000000 + ${REFERENCE_ASSET}" | tr -dc '0-9')
reference_address_out="${cip68_script_address} + ${UTXO_VALUE} + ${REFERENCE_ASSET}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file ./data/sale-datum.json \
    --tx-out="${sale_script_address} + 5000000 + ${FRACTION_ASSET}" | tr -dc '0-9')
fraction_address_out="${sale_script_address} + ${UTXO_VALUE} + ${FRACTION_ASSET}"

echo "Reference Mint OUTPUT:" ${reference_address_out}
echo "Fraction Mint OUTPUT:" ${fraction_address_out}
#
exit
#
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json
TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/mint-reference-utxo.signed)

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${newm_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${newm_tx_in} \
    --tx-out="${reference_address_out}" \
    --tx-out-inline-datum-file ./data/metadata-datum.json \
    --tx-out="${fraction_address_out}" \
    --tx-out-inline-datum-file ./data/sale-datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${newm_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/mint_redeemer.json \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/newm-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
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

tx=$(cardano-cli transaction txid --tx-file tmp/tx.signed)
echo "Tx Hash:" $tx