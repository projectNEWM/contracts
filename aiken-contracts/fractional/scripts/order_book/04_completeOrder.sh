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
TXIN=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in1=${TXIN::-8}
TXIN=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in2=${TXIN::-8}
echo First UTxO: $script_tx_in1
echo Second UTxO: $script_tx_in2

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
echo Current Lovelace: $lovelace_value1
current_have_value1=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" --arg pid "${pid1}" --arg tkn "${tkn1}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
current_incentive_value1=$(jq -r --arg alltxin "" --arg pkh "${buyer1_pkh}" --arg pid "${ipid1}" --arg tkn "${itkn1}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)

# next 
lovelace_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${pid2}" --arg tkn "${tkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value.lovelace' ../tmp/script_utxo.json)
echo Current Lovelace: $lovelace_value2
current_have_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${pid2}" --arg tkn "${tkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
current_incentive_value2=$(jq -r --arg alltxin "" --arg pkh "${buyer2_pkh}" --arg pid "${ipid2}" --arg tkn "${itkn2}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $pkh) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)


if [[ $current_have_value1 -le 0 && $current_incentive_value1 -le 0 ]]; then
    exit
elif [[ $current_have_value1 -le 0 && $current_incentive_value1 -gt 0 ]]; then
    exit
elif [[ $current_have_value1 -gt 0 && $current_incentive_value1 -le 0 ]]; then
    exit
else
    returning_asset="4312688 ${pid2}.${tkn2}"
    # returning_asset="${current_have_value1} ${pid1}.${tkn1}"
    # returning_asset="${current_have_value1} ${pid1}.${tkn1} + ${current_incentive_value1} ${ipid1}.${itkn1}"
    script_address_out1="${script_address} + ${lovelace_value1} + ${returning_asset}"
fi
echo "Complete OUTPUT 1: "${script_address_out1}

if [[ $current_have_value2 -le 0 && $current_incentive_value2 -le 0 ]]; then
    exit
elif [[ $current_have_value2 -le 0 && $current_incentive_value2 -gt 0 ]]; then
    exit
elif [[ $current_have_value2 -gt 0 && $current_incentive_value2 -le 0 ]]; then
    exit
else
    returning_asset="3234516 ${pid1}.${tkn1} + 3009546 ${pid2}.${tkn2}"
    # returning_asset="${current_have_value2} ${pid2}.${tkn2}"
    # returning_asset="${current_have_value2} ${pid2}.${tkn2} + ${current_incentive_value2} ${ipid2}.${itkn2}"
    script_address_out2="${script_address} + ${lovelace_value2} + ${returning_asset}"
fi
echo "Complete OUTPUT 2: "${script_address_out2}


#
# exit
#

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
batcher_tx_in=${TXIN::-8}
echo Batcher UTXO ${batcher_tx_in}

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
    --change-address ${batcher_address} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${script_tx_in1} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer1.json \
    --tx-in ${script_tx_in2} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/order_book/complete-redeemer2.json \
    --tx-out="${script_address_out1}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum1.json  \
    --tx-out="${script_address_out2}" \
    --tx-out-inline-datum-file ../data/order_book/order-book-datum2.json  \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
#

# echo -e "\033[0;36m Gathering Batcher UTxO Information  \033[0m"
# ${cli} query utxo \
#     --address ${batcher_address} \
#     --testnet-magic ${testnet_magic} \
#     --out-file ../tmp/batcher_utxo.json
# # transaction variables
# TXNS=$(jq length ../tmp/batcher_utxo.json)
# if [ "${TXNS}" -eq "0" ]; then
#    echo -e "\n \033[0;31m NO UTxOs Found At ${batcher_address} \033[0m \n";
#    exit;
# fi

# TXIN=$(jq -r --arg alltxin "" 'to_entries[] | .key | . + $alltxin + " --tx-in"' ../tmp/batcher_utxo.json)
# batcher_starting_lovelace=$(jq '[.[] | .value.lovelace] | add' ../tmp/batcher_utxo.json)
# batcher_tx_in=${TXIN::-8}
# echo Batcher UTXO ${batcher_tx_in}
# # exit

# echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
# ${cli} query utxo \
#     --address ${queue_script_address} \
#     --testnet-magic ${testnet_magic} \
#     --out-file ../tmp/queue_script_utxo.json
# # transaction variables
# TXNS=$(jq length ../tmp/queue_script_utxo.json)
# if [ "${TXNS}" -eq "0" ]; then
#    echo -e "\n \033[0;31m NO UTxOs Found At ${order_book_script_address} \033[0m \n";
#    exit;
# fi

# TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${buyer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $artistPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/queue_script_utxo.json)
# queue_tx_in=${TXIN::-8}
# echo QUEUE UTXO ${queue_tx_in}

# # exit

# echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
# ${cli} query utxo \
#     --address ${order_book_script_address} \
#     --testnet-magic ${testnet_magic} \
#     --out-file ../tmp/order_book_script_utxo.json
# # transaction variables
# TXNS=$(jq length ../tmp/order_book_script_utxo.json)
# if [ "${TXNS}" -eq "0" ]; then
#    echo -e "\n \033[0;31m NO UTxOs Found At ${order_book_script_address} \033[0m \n";
#    exit;
# fi
# #
# pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
# tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
# total_amt=100000000

# TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${buyer_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .key' ../tmp/order_book_script_utxo.json)
# order_book_tx_in=$TXIN
# echo SALE UTXO ${order_book_tx_in}

# pointer_pid=$(cat ../../hashes/pointer_policy.hash)
# pointer_tkn=$(cat ../tmp/pointer.token)
# pointer_asset="1 ${pointer_pid}.${pointer_tkn}"

# default_asset="${total_amt} ${pid}.${tkn}"
# CURRENT_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${buyer_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value[$pid][$tkn]' ../tmp/order_book_script_utxo.json)
# echo REMAINING: $CURRENT_VALUE ${pid}.${tkn}

# # how many bundles to purchase
# bundleSize=$(jq -r '.fields[2].int' ../data/queue/queue-datum.json)

# # need to build the cost object and calculate the total cost
# cost_value=$(python3 -c "import sys; sys.path.append('../py/'); from convertMapToOutput import get_map; get_map($(jq -r '.fields[2].map' ../data/sale/sale-datum.json), ${bundleSize})")

# # the bundle size
# bSize=$(jq -r '.fields[1].fields[2].int' ../data/sale/sale-datum.json)

# # the pure ada part
# pSize=$(jq '.fields[2].map[] | select(.k.bytes == "") | .v.map[].v.int' ../data/sale/sale-datum.json)
# if [[ -z $pSize ]]; then
#   pSize=0
# fi
# payAmt=$((${bundleSize} * ${pSize}))

# buyAmt=$((${bundleSize} * ${bSize}))
# retAmt=$((${CURRENT_VALUE} - ${buyAmt}))

# if [[ CURRENT_VALUE -lt buyAmt ]] ; then
#     echo "Partial Fill"
#     retAmt=${CURRENT_VALUE}
# fi

# # buyer info
# bundle_value="${buyAmt} ${pid}.${tkn}"

# returning_asset="${retAmt} ${pid}.${tkn}"

# incentive="1000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"
# batcher_address_out="${batcher_address} + ${batcher_starting_lovelace} + ${incentive}"

# # need better way for this
# cost_value="20000000 015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.43757272656e6379"

# # queue contract return
# # the cost value is ada
# queue_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/queue_script_utxo.json)
# order_book_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/order_book_script_utxo.json)
# queue_ada_return=$((${queue_utxo_value} - ${payAmt}))
# order_book_ada_return=$((${order_book_utxo_value} + ${payAmt}))
# if [ -z "$cost_value" ]; then
#     echo "cost value is empty"

#     if [[ retAmt -le 0 ]] ; then
#         # echo "THIS CLEANS THE SALE OUT"
#         order_book_script_address_out="${order_book_script_address} + ${order_book_ada_return} + ${pointer_asset}"
#         queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value}"
#         # echo $order_book_script_address_out
#         # exit
#     else
#         # echo "somethig to continue" ${returning_asset}
#         queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value}"
#         order_book_script_address_out="${order_book_script_address} + ${order_book_ada_return} + ${returning_asset} + ${pointer_asset}"
#     fi
# else
#     echo "cost value isnt empty"
#     queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value}"
#     if [[ retAmt -le 0 ]] ; then
#         order_book_script_address_out="${order_book_script_address} + ${order_book_ada_return} + ${cost_value} + ${pointer_asset}"
#     else
#         order_book_script_address_out="${order_book_script_address} + ${order_book_ada_return} + ${returning_asset} + ${cost_value} + ${pointer_asset}"
#     fi
# fi

# echo "Batcher OUTPUT: "${batcher_address_out}
# echo "Sale Script OUTPUT: "${order_book_script_address_out}
# echo "Queue Script OUTPUT: "${queue_script_address_out}
# #
# # exit
# #

# # collat info
# echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
# ${cli} query utxo \
#     --testnet-magic ${testnet_magic} \
#     --address ${collat_address} \
#     --out-file ../tmp/collat_utxo.json
# TXNS=$(jq length ../tmp/collat_utxo.json)
# if [ "${TXNS}" -eq "0" ]; then
#    echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
#    exit;
# fi
# collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

# script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/sale-reference-utxo.signed )
# queue_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/queue-reference-utxo.signed )
# data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )


# cpu_steps=300000000
# mem_steps=1000000

# order_book_execution_unts="(${cpu_steps}, ${mem_steps})"
# order_book_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
# order_book_computation_fee_int=$(printf "%.0f" "$order_book_computation_fee")

# cpu_steps=1000000000
# mem_steps=3000000
# queue_execution_unts="(${cpu_steps}, ${mem_steps})"
# queue_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
# queue_computation_fee_int=$(printf "%.0f" "$queue_computation_fee")

# # Add metadata to this build function for nfts with data
# echo -e "\033[0;36m Building Tx \033[0m"
# # change_value=$((${queue_ada_return} - 375629))
# # queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}"

# echo -e "\033[0;36m Building Tx \033[0m"
# ${cli} transaction build-raw \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --out-file ../tmp/tx.draft \
#     --tx-in-collateral="${collat_utxo}" \
#     --read-only-tx-in-reference="${data_ref_utxo}#0" \
#     --tx-in ${batcher_tx_in} \
#     --tx-in ${order_book_tx_in} \
#     --spending-tx-in-reference="${script_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
#     --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
#     --tx-in ${queue_tx_in} \
#     --spending-tx-in-reference="${queue_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-execution-units="${queue_execution_unts}" \
#     --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
#     --tx-out="${batcher_address_out}" \
#     --tx-out="${order_book_script_address_out}" \
#     --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
#     --tx-out="${queue_script_address_out}" \
#     --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
#     --required-signer-hash ${batcher_pkh} \
#     --required-signer-hash ${collat_pkh} \
#     --fee 400000


# FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
# fee=$(echo $FEE | rev | cut -c 9- | rev)

# total_fee=$((${fee} + ${order_book_computation_fee_int} + ${queue_computation_fee_int}))
# echo Tx Fee: $total_fee
# change_value=$((${queue_ada_return} - ${total_fee}))
# queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}"
# echo "Without Fee: Queue Script OUTPUT: "${queue_script_address_out}

# # exit

# ${cli} transaction build-raw \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --out-file ../tmp/tx.draft \
#     --tx-in-collateral="${collat_utxo}" \
#     --read-only-tx-in-reference="${data_ref_utxo}#0" \
#     --tx-in ${batcher_tx_in} \
#     --tx-in ${order_book_tx_in} \
#     --spending-tx-in-reference="${script_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-execution-units="${order_book_execution_unts}" \
#     --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
#     --tx-in ${queue_tx_in} \
#     --spending-tx-in-reference="${queue_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-execution-units="${queue_execution_unts}" \
#     --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
#     --tx-out="${batcher_address_out}" \
#     --tx-out="${order_book_script_address_out}" \
#     --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
#     --tx-out="${queue_script_address_out}" \
#     --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
#     --required-signer-hash ${batcher_pkh} \
#     --required-signer-hash ${collat_pkh} \
#     --fee ${total_fee}



# IFS=':' read -ra VALUE <<< "${FEE}"
# IFS=' ' read -ra FEE <<< "${VALUE[1]}"
# FEE=${FEE[1]}
# echo -e "\033[1;32m Fee: \033[0m" $FEE
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
