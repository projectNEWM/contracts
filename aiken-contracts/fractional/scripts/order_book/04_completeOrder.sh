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

buyer1_address=$(cat ../wallets/buyer1-wallet/payment.addr)
buyer1_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer1-wallet/payment.vkey)

buyer2_address=$(cat ../wallets/buyer2-wallet/payment.addr)
buyer2_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer2-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)


# get the current value
pid1=$(jq -r '.fields[1].fields[0].bytes' ../data/order_book/order-book-datum1.json)
tkn1=$(jq -r '.fields[1].fields[1].bytes' ../data/order_book/order-book-datum1.json)

ipid1=$(jq -r '.fields[3].fields[0].bytes' ../data/order_book/order-book-datum1.json)
itkn1=$(jq -r '.fields[3].fields[1].bytes' ../data/order_book/order-book-datum1.json)

# get the current value
pid2=$(jq -r '.fields[1].fields[0].bytes' ../data/order_book/order-book-datum2.json)
tkn2=$(jq -r '.fields[1].fields[1].bytes' ../data/order_book/order-book-datum2.json)

ipid2=$(jq -r '.fields[3].fields[0].bytes' ../data/order_book/order-book-datum2.json)
itkn2=$(jq -r '.fields[3].fields[1].bytes' ../data/order_book/order-book-datum2.json)

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
TXIN=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in1=${TXIN::-8}
TXIN=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in2=${TXIN::-8}

# we can calculate the trade here

first_assets=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from order_book import get_this_amt; get_this_amt('../tmp/script_utxo.json', '${script_tx_in1}', '${script_tx_in2}')")
second_assets=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from order_book import get_that_amt; get_that_amt('../tmp/script_utxo.json', '${script_tx_in1}', '${script_tx_in2}')")

# echo $first_assets
# echo $second_assets
# exit

id1=${script_tx_in1::-2}
idx1=${script_tx_in1: -1}

id2=${script_tx_in2::-2}
idx2=${script_tx_in2: -1}

jq -r \
--arg id "$id1" \
--argjson idx "$idx1" \
'.fields[0].fields[0].bytes=$id | .fields[0].fields[1].int=$idx' \
../data/order_book/complete-redeemer2.json | sponge ../data/order_book/complete-redeemer2.json

jq -r \
--arg id "$id2" \
--argjson idx "$idx2" \
'.fields[0].fields[0].bytes=$id | .fields[0].fields[1].int=$idx' \
../data/order_book/complete-redeemer1.json | sponge ../data/order_book/complete-redeemer1.json

lovelace_value1=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" --arg pid "${pid1}" --arg tkn "${tkn1}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value.lovelace' ../tmp/script_utxo.json)
# echo Current Lovelace: $lovelace_value1
current_have_value1=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" --arg pid "${pid1}" --arg tkn "${tkn1}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
current_incentive_value1=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" --arg pid "${ipid1}" --arg tkn "${itkn1}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)

# next 
lovelace_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${pid2}" --arg tkn "${tkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value.lovelace' ../tmp/script_utxo.json)
# echo Current Lovelace: $lovelace_value2
current_have_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${pid2}" --arg tkn "${tkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
current_incentive_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${ipid2}" --arg tkn "${itkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)


# connect teh pythong script to calculate the amounts

if [[ $current_have_value1 -le 0 && $current_incentive_value1 -le 0 ]]; then
    exit
elif [[ $current_have_value1 -le 0 && $current_incentive_value1 -gt 0 ]]; then
    exit
elif [[ $current_have_value1 -gt 0 && $current_incentive_value1 -le 0 ]]; then
    exit
else
    script_address_out1="${script_address} + ${lovelace_value1} + ${first_assets}"
fi
echo First UTxO: $script_tx_in1
echo "Complete OUTPUT 1: "${script_address_out1}

if [[ $current_have_value2 -le 0 && $current_incentive_value2 -le 0 ]]; then
    exit
elif [[ $current_have_value2 -le 0 && $current_incentive_value2 -gt 0 ]]; then
    exit
elif [[ $current_have_value2 -gt 0 && $current_incentive_value2 -le 0 ]]; then
    exit
else
    script_address_out2="${script_address} + ${lovelace_value2} + ${second_assets}"
fi
echo
echo Second UTxO: $script_tx_in2
echo "Complete OUTPUT 2: "${script_address_out2}

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
echo Batcher UTXO ${batcher_tx_in}

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

cpu_steps=950000000
mem_steps=3000000

order_book_execution_unts="(${cpu_steps}, ${mem_steps})"
order_book_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
order_book_computation_fee_int=$(printf "%.0f" "$order_book_computation_fee")

computational_fee=$((2 * ${order_book_computation_fee_int}))
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
    --tx-in ${script_tx_in1} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer1.json \
    --tx-in ${script_tx_in2} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer2.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${script_address_out1}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum1.json  \
    --tx-out="${script_address_out2}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum2.json  \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee 400000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)
total_fee=$((${fee} + ${computational_fee}))

if (( total_fee % 2 == 0 )); then
  final_fee=$total_fee
else
  final_fee=$((1 + ${total_fee}))
fi

split_fee=$((${final_fee} / 2))

echo Tx Fee: $final_fee

script_address_out1="${script_address} + $((${lovelace_value1} - ${split_fee})) + ${first_assets}"
script_address_out2="${script_address} + $((${lovelace_value2} - ${split_fee})) + ${second_assets}"

echo "OUTPUT 1: "${script_address_out1}
echo "OUTPUT 2: "${script_address_out2}

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${script_tx_in1} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer1.json \
    --tx-in ${script_tx_in2} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer2.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${script_address_out1}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum1.json  \
    --tx-out="${script_address_out2}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum2.json  \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee ${final_fee}

#
exit
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
