#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# bundle sale contract
sale_script_path="../../contracts/sale_contract.plutus"
sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# bundle sale contract
queue_script_path="../../contracts/queue_contract.plutus"
queue_script_address=$(${cli} address build --payment-script-file ${queue_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# collat, artist, reference
#
newm_address=$(cat ../wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

batcher_address=$(cat ../wallets/batcher-wallet/payment.addr)
batcher_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/batcher-wallet/payment.vkey)


artist_address=$(cat ../wallets/artist-wallet/payment.addr)
artist_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

#
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${queue_script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/queue_script_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/queue_script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sale_script_address} \033[0m \n";
   exit;
fi

TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${buyer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $artistPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/queue_script_utxo.json)
queue_tx_in=${TXIN::-8}
echo QUEUE UTXO ${queue_tx_in}

# exit

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${sale_script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/sale_script_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/sale_script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sale_script_address} \033[0m \n";
   exit;
fi
#
pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
total_amt=100000000

TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .key' ../tmp/sale_script_utxo.json)
sale_tx_in=$TXIN
echo SALE UTXO ${sale_tx_in}

default_asset="${total_amt} ${pid}.${tkn}"
CURRENT_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value[$pid][$tkn]' ../tmp/sale_script_utxo.json)
echo REMAINING: $CURRENT_VALUE ${pid}.${tkn}

# how many bundles to purchase
bundleSize=$(jq -r '.fields[2].int' ../data/queue/queue-datum.json)

# need to build the cost object and calculate the total cost
cost_value=$(python3 -c "import sys; sys.path.append('../py/'); from convertMapToOutput import get_map; get_map($(jq -r '.fields[2].map' ../data/sale/sale-datum.json), ${bundleSize})")

# the bundle size
bSize=$(jq -r '.fields[1].fields[2].int' ../data/sale/sale-datum.json)

# the pure ada part
pSize=$(jq '.fields[2].map[] | select(.k.bytes == "") | .v.map[].v.int' ../data/sale/sale-datum.json)
payAmt=$((${bundleSize} * ${pSize}))

buyAmt=$((${bundleSize} * ${bSize}))
retAmt=$((${CURRENT_VALUE} - ${buyAmt}))

if [[ CURRENT_VALUE -lt buyAmt ]] ; then
    echo "Partial Fill"
    retAmt=${CURRENT_VALUE}
fi

# buyer info
bundle_value="${buyAmt} ${pid}.${tkn}"

returning_asset="${retAmt} ${pid}.${tkn}"

incentive=1000000
batcher_address_out="${batcher_address} + ${incentive}"

# queue contract return
# the cost value is ada
if [ -z "$cost_value" ]; then
    queue_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/queue_script_utxo.json)
    queue_ada_return=$((${queue_utxo_value} - ${payAmt} - ${incentive}))
    sale_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/sale_script_utxo.json)
    sale_ada_return=$((${sale_utxo_value} + ${payAmt}))

    if [[ retAmt -le 0 ]] ; then
        # echo "THIS CLEANS THE SALE OUT"
        sale_script_address_out="${sale_script_address} + ${sale_ada_return}"
        queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value}"
        # echo $sale_script_address_out
        # exit
    else
        # echo "somethig to continue" ${returning_asset}
        queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value}"
        sale_script_address_out="${sale_script_address} + ${sale_ada_return} + ${returning_asset}"
    fi
fi

echo "Batcher OUTPUT: "${batcher_address_out}
echo "Sale Script OUTPUT: "${sale_script_address_out}
echo "Queue Script OUTPUT: "${queue_script_address_out}

# echo "Sale Script OUTPUT: "${sale_script_address_out}
# echo "Queue Script OUTPUT: "${queue_script_address_out}


# exit
# queue_utxo_value=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --tx-out-inline-datum-file ../data/queue/queue-datum.json \
#     --tx-out="${queue_script_address} + 5000000 + ${buyer_assets}" | tr -dc '0-9')
# queue_script_address_out="${queue_script_address}"
# # sale contract return
# sale_script_address_out="${sale_script_address}"

# if [ -z "$cost_value" ]; then
#     adaPay=$((${artist_utxo_value} + ${payAmt}))
#     script_address_out="${sale_script_address} + ${adaPay}"
# else
#     artist_utxo_value=$(${cli} transaction calculate-min-required-utxo \
#         --babbage-era \
#         --protocol-params-file ../tmp/protocol.json \
#         --tx-out="${artist_address} + 5000000 + ${cost_value}" | tr -dc '0-9')

#     pSize=$(jq '.fields[2].map[] | select(.k.bytes == "") | .v.map[].v.int' ../data/sale/sale-datum.json)
#     adaPay=$((${artist_utxo_value} + ${payAmt}))
#     artist_address_out="${artist_address} + ${adaPay} + ${cost_value}"
# fi

# echo $cost_value $adaPay

# exit 



# buyer_utxo_value=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --tx-out="${sale_script_address} + 5000000 + ${buyer_asset}" | tr -dc '0-9')

# buyer_address_out="${buyer_address} + ${buyer_utxo_value} + ${buyer_asset}"

# # script info
# script_utxo_value=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --tx-out-inline-datum-file ../data/sale/sale-datum.json \
#     --tx-out="${sale_script_address} + 5000000 + ${default_asset}" | tr -dc '0-9')

# returning_asset="${retAmt} ${pid}.${tkn}"

# if [[ retAmt -le 0 ]] ; then
#     script_address_out="${sale_script_address} + ${script_utxo_value}"
# else
#     script_address_out="${sale_script_address} + ${script_utxo_value} + ${returning_asset}"
# fi




# echo "Script OUTPUT: "${script_address_out}
# echo "Buyer OUTPUT: "${buyer_address_out}
# echo "Seller OUTPUT: "${artist_address_out}
#
# exit
#
# Get tx payer info
# echo -e "\033[0;36m Gathering Batcher Bot UTxO Information  \033[0m"
# ${cli} query utxo \
#     --testnet-magic ${testnet_magic} \
#     --address ${buyer_address} \
#     --out-file ../tmp/buyer_utxo.json

# TXNS=$(jq length ../tmp/buyer_utxo.json)
# if [ "${TXNS}" -eq "0" ]; then
#    echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
#    exit;
# fi
# alltxin=""
# TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ../tmp/buyer_utxo.json)
# buyer_tx_in=${TXIN::-8}

# collat info
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

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/sale-reference-utxo.signed )
queue_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/queue-reference-utxo.signed )
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )


cpu_steps=200000000
mem_steps=1000000

sale_execution_unts="(${cpu_steps}, ${mem_steps})"
sale_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
sale_computation_fee_int=$(printf "%.0f" "$sale_computation_fee")

cpu_steps=800000000
mem_steps=3000000
queue_execution_unts="(${cpu_steps}, ${mem_steps})"
queue_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
queue_computation_fee_int=$(printf "%.0f" "$queue_computation_fee")

# # Add metadata to this build function for nfts with data
# echo -e "\033[0;36m Building Tx \033[0m"
# change_value=$((${queue_ada_return} - 375629))
# queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}"
# FEE=$(${cli} transaction build \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --out-file ../tmp/tx.draft \
#     --change-address ${newm_address} \
#     --tx-in-collateral="${collat_utxo}" \
#     --tx-in ${sale_tx_in} \
#     --spending-tx-in-reference="${script_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
#     --tx-in ${queue_tx_in} \
#     --spending-tx-in-reference="${queue_ref_utxo}#1" \
#     --spending-plutus-script-v2 \
#     --spending-reference-tx-in-inline-datum-present \
#     --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
#     --tx-out="${sale_script_address_out}" \
#     --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
#     --tx-out="${queue_script_address_out}" \
#     --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
#     --read-only-tx-in-reference="${data_ref_utxo}#0" \
#     --required-signer-hash ${newm_pkh} \
#     --required-signer-hash ${collat_pkh} \
#     --testnet-magic ${testnet_magic})

# exit

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
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
    --fee 400000


FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 2 --tx-out-count 2 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)

total_fee=$((${fee} + ${sale_computation_fee_int} + ${queue_computation_fee_int}))
echo Tx Fee: $total_fee
change_value=$((${queue_ada_return} - ${total_fee}))
queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}"
echo "Without Fee: Queue Script OUTPUT: "${queue_script_address_out}

# exit

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
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
