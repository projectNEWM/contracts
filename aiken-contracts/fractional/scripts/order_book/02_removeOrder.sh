#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# bundle sale contract
order_book_script_path="../../contracts/order_book_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${order_book_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# set the buyer
buyer="buyer2"
buyer_address=$(cat ../wallets/${buyer}-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/${buyer}-wallet/payment.vkey)

if [ "$buyer" = "buyer1" ]; then
    datum_path="../data/order_book/order-book-datum1.json"
elif [ "$buyer" = "buyer2" ]; then
    datum_path="../data/order_book/order-book-datum2.json"
else
    exit
fi

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# get the current
pid=$(jq -r '.fields[1].fields[0].bytes' ${datum_path})
tkn=$(jq -r '.fields[1].fields[1].bytes' ${datum_path})

ipid=$(jq -r '.fields[3].fields[0].bytes' ${datum_path})
itkn=$(jq -r '.fields[3].fields[1].bytes' ${datum_path})

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/script_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
TXIN=$(jq -r --arg alltxin "" --arg pkh "${buyer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}
echo $script_tx_in

lovelace_value=$(jq -r --arg alltxin "" --arg pkh "${buyer_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value.lovelace' ../tmp/script_utxo.json)
echo Current Lovelace: $lovelace_value

# exit
current_have_value=$(jq -r --arg alltxin "" --arg pkh "${buyer_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
current_incentive_value=$(jq -r --arg alltxin "" --arg pkh "${buyer_pkh}" --arg pid "${ipid}" --arg tkn "${itkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)

if [[ $current_have_value -le 0 && $current_incentive_value -le 0 ]]; then
    buyer_address_out="${buyer_address} + ${lovelace_value}"
elif [[ $current_have_value -le 0 && $current_incentive_value -gt 0 ]]; then
    returning_asset="${current_incentive_value} ${ipid}.${itkn}"
    buyer_address_out="${buyer_address} + ${lovelace_value} + ${returning_asset}"
elif [[ $current_have_value -gt 0 && $current_incentive_value -le 0 ]]; then
    returning_asset="${current_have_value} ${pid}.${tkn}"
    buyer_address_out="${buyer_address} + ${lovelace_value} + ${returning_asset}"
else
    returning_asset="${current_have_value} ${pid}.${tkn} + ${current_incentive_value} ${ipid}.${itkn}"
    buyer_address_out="${buyer_address} + ${lovelace_value} + ${returning_asset}"
fi

echo "Return OUTPUT: "${buyer_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${buyer_address} \
    --out-file ../tmp/buyer_utxo.json
TXNS=$(jq length ../tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/buyer_utxo.json)
buyer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file ../tmp/collat_utxo.json
TXNS=$(jq length ../tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/order-book-reference-utxo.signed )
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${buyer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/order_book/remove-redeemer.json \
    --tx-out="${buyer_address_out}" \
    --required-signer-hash ${buyer_pkh} \
    --required-signer-hash ${collat_pkh} \
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
    --signing-key-file ../wallets/${buyer}-wallet/payment.skey \
    --signing-key-file ../wallets/collat-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx.signed

tx=$(cardano-cli transaction txid --tx-file ../tmp/tx.signed)
echo "Tx Hash:" $tx