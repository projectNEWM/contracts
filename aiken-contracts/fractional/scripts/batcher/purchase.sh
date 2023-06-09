#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

batcher_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/batcher-wallet/payment.vkey)


if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Requries Args \033[0m \n";
    # exit
fi

# inputs
batcher_tx_in=${1}
sale_tx_in=${2}
queue_tx_in=${3}

# references
data_ref_utxo=${4}
sale_ref_utxo=${5}
queue_ref_utxo=${6}

# outputs
batcher_address_out=${7}
sale_address_out=${8}
queue_address_out=${9}

# fee stuff
cpu_steps=300000000
mem_steps=1000000

sale_execution_unts="(${cpu_steps}, ${mem_steps})"
sale_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
sale_computation_fee_int=$(printf "%.0f" "$sale_computation_fee")

cpu_steps=1000000000
mem_steps=3000000
queue_execution_unts="(${cpu_steps}, ${mem_steps})"
queue_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
queue_computation_fee_int=$(printf "%.0f" "$queue_computation_fee")

echo $sale_execution_unts
echo $queue_execution_unts
# echo $queue_computation_fee_int
# echo $(($sale_computation_fee_int + $queue_computation_fee_int))
exit 
# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"


echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${batcher_tx_in}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${sale_tx_in} \
    --spending-tx-in-reference="${sale_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${sale_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
    --tx-in ${queue_tx_in} \
    --spending-tx-in-reference="${queue_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${queue_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${sale_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
    --tx-out="${queue_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
    --required-signer-hash ${batcher_pkh} \
    --fee 400000


FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)

total_fee=$((${fee} + ${sale_computation_fee_int} + ${queue_computation_fee_int}))
echo Tx Fee: $total_fee
change_value=$((${queue_ada_return} - ${total_fee}))
queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}"
echo "Without Fee: Queue Script OUTPUT: "${queue_script_address_out}

exit

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${batcher_tx_in} \
    --tx-in ${sale_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${sale_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
    --tx-in ${queue_tx_in} \
    --spending-tx-in-reference="${queue_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${queue_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${sale_script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
    --tx-out="${queue_script_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee ${total_fee}
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/batcher-wallet/payment.skey \
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
echo -n ${tx} > ../tmp/last-sale.txhash
cp ../tmp/tx.signed ../tmp/last-sale-utxo.signed