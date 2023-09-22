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

# collat, artist, reference
#
batcher_address=$(cat ../wallets/batcher-wallet/payment.addr)
batcher_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/batcher-wallet/payment.vkey)

# set the buyer
buyer="buyer1"
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


# get the current value
hpid=$(jq -r '.fields[1].fields[0].bytes' ${datum_path})
htkn=$(jq -r '.fields[1].fields[1].bytes' ${datum_path})

wpid=$(jq -r '.fields[2].fields[0].fields[0].bytes' ${datum_path})
wtkn=$(jq -r '.fields[2].fields[0].fields[1].bytes' ${datum_path})

echo -e "\033[0;36m Gathering Script UTxO Information\n\033[0m"
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
echo Script UTXO: ${script_tx_in}


lovelace_value=$(jq -r --arg pkh "${buyer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value.lovelace' ../tmp/script_utxo.json)
# echo Current Lovelace: $lovelace_value
current_have_value=$(jq -r --arg pkh "${buyer_pkh}" --arg pid "${hpid}" --arg tkn "${htkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
# echo Current Have: $current_have_value
current_want_value=$(jq -r --arg pkh "${buyer_pkh}" --arg pid "${wpid}" --arg tkn "${wtkn}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
# echo Current Want: $current_want_value

if [ "$current_have_value" = "null" ]; then
    # echo "Value is null"
    have_token=""
else
    # echo "Value is not null or empty: $current_have_value"
    have_token=" + ${current_have_value} ${hpid}.${htkn}"
fi

if [ "$current_want_value" = "null" ]; then
    # echo "Value is null"
    want_token=""
else
    # echo "Value is not null or empty: $current_want_value"
    want_token=" + ${current_want_value} ${wpid}.${wtkn}"

fi

buyer_address_out="${buyer_address} + ${lovelace_value}${have_token}${want_token}"
echo Buyer Out: ${buyer_address_out}

echo -e "\033[0;36m Gathering Batcher UTxO Information  \033[0m"
${cli} query utxo \
    --address ${batcher_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/batcher_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/batcher_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${batcher_address} \033[0m \n";
   exit;
fi

TXIN=$(jq -r --arg alltxin "" 'to_entries[] | .key | . + $alltxin + " --tx-in"' ../tmp/batcher_utxo.json)
batcher_starting_lovelace=$(jq '[.[] | .value.lovelace] | add' ../tmp/batcher_utxo.json)
batcher_starting_incentive=$(jq '[.[] | .value["698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d"]["7444524950"]] | add' ../tmp/batcher_utxo.json)
batcher_tx_in=${TXIN::-8}
echo Batcher UTXO: ${batcher_tx_in}

incentive="$((${current_incentive_value1} + ${current_incentive_value2} + ${batcher_starting_incentive})) 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"

token_name="5ca1ab1e000affab1e000ca11ab1e0005e77ab1e"
batcher_policy_id=$(cat ../../hashes/batcher.hash)
batcher_token="1 ${batcher_policy_id}.${token_name}"
batcher_address_out="${batcher_address} + ${batcher_starting_lovelace} + ${incentive} + ${batcher_token}"
echo -e "\nBatcher OUTPUT: "${batcher_address_out}

#
# exit
#

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

cpu_steps=550000000
mem_steps=2000000

order_book_execution_unts="(${cpu_steps}, ${mem_steps})"
order_book_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
order_book_computation_fee_int=$(printf "%.0f" "$order_book_computation_fee")

computational_fee=$((1 * ${order_book_computation_fee_int}))
# echo Tx Fee: $computational_fee

# exit

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/refund-redeemer.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${buyer_address_out}" \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee 400000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)
total_fee=$((${fee} + ${computational_fee}))
echo Tx Fee: $total_fee

buyer_address_out="${buyer_address} + $((${lovelace_value} - ${total_fee}))${have_token}${want_token}"

echo "Buyer OUTPUT: "${buyer_address_out}

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/refund-redeemer.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${buyer_address_out}" \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee ${total_fee}

#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/batcher-wallet/payment.skey \
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

cp ../tmp/tx.signed ../tmp/last-sale-utxo.signed
