#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# bundle sale contract
sale_script_path="../../contracts/sale_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# collat, artist, reference
artist_address=$(cat ../wallets/artist-wallet/payment.addr)
artist_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

# pointer minter key
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

pointer_pid=$(cat ../../hashes/pointer_policy.hash)
pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
pointer_tkn=$(cat ../tmp/pointer.token)
total_amt=100000000

# echo $tkn

default_asset="${total_amt} ${pid}.${tkn}"

utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --tx-out="${script_address} + 5000000 + ${default_asset}" | tr -dc '0-9')

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
TXIN=$(jq -r --arg alltxin "" --arg tkn "${tkn}" 'to_entries[] | select(.value.inlineDatum.fields[1].fields[1].bytes == $tkn) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}
echo $script_tx_in

# exit
LOVELACE_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value.lovelace' ../tmp/script_utxo.json)
utxo_value=$LOVELACE_VALUE
echo LOVELACE: $LOVELACE_VALUE
CURRENT_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
returning_asset="${CURRENT_VALUE} ${pid}.${tkn}"

POINTER_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pointer_pid}" --arg tkn "${pointer_tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)

if [ ! -z "$POINTER_VALUE" ]; then
    echo "UTxO has pointer."
    exit 1
fi
prefix_555="0022bfb0"

echo "NEWM UTxO:" $newm_tx_in
first_utxo=$(jq -r 'keys[0]' ../tmp/script_utxo.json)
string=${first_utxo}
IFS='#' read -ra array <<< "$string"

point_name=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_555}')")
echo -n $point_name > ../tmp/pointer.token

pointer_asset="1 ${pointer_pid}.${point_name}"

# echo '{"${pointer_pid}":{"${point_name}": 1}}' > ../data/sale/pointer.json
echo "{\"${pointer_pid}\":{\"${point_name}\": 1}}" > ../data/sale/pointer.json


# the cost of a bundle is defined in the sale data folder
value_map=$(python3 -c "import sys; sys.path.append('../py/'); from convertCostToMap import map_cost_file; map_cost_file('../data/sale/pointer.json')")

# update the value map inside the start redeemer
jq \
--argjson value_map "$value_map" \
'.fields[0].map=$value_map' \
../data/sale/start-redeemer.json | sponge ../data/sale/start-redeemer.json

# compute the correct start redeemer 

script_address_out="${script_address} + ${utxo_value} + ${returning_asset} + ${pointer_asset}"
echo $script_address_out
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${artist_address} \
    --out-file ../tmp/artist_utxo.json
TXNS=$(jq length ../tmp/artist_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${artist_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/artist_utxo.json)
artist_tx_in=${TXIN::-8}

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
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )
pointer_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/pointer-reference-utxo.signed)

# CPU MEM UNITS
cpu_steps=350000000
mem_steps=1000000
# sale
sale_execution_unts="(${cpu_steps}, ${mem_steps})"
sale_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
sale_computation_fee_int=$(printf "%.0f" "$sale_computation_fee")

cpu_steps=150000000
mem_steps=500000
# pointer minter
pointer_execution_unts="(${cpu_steps}, ${mem_steps})"
pointer_computation_fee=$(echo "0.0000721*${cpu_steps} + 0.0577*${mem_steps}" | bc)
pointer_computation_fee_int=$(printf "%.0f" "$pointer_computation_fee")

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --protocol-params-file ../tmp/protocol.json \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${sale_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/sale/start-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --mint="${pointer_asset}" \
    --mint-tx-in-reference="${pointer_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${pointer_pid}" \
    --mint-reference-tx-in-execution-units="${pointer_execution_unts}" \
    --mint-reference-tx-in-redeemer-file ../data/mint/mint-redeemer.json \
    --required-signer-hash ${newm_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee 400000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)

total_fee=$((${fee} + ${sale_computation_fee_int} + ${pointer_computation_fee_int}))
echo Tx Fee: $total_fee
change_value=$((${LOVELACE_VALUE} - ${total_fee}))
script_address_out="${script_address} + ${change_value} + ${returning_asset} + ${pointer_asset}"
echo "Return OUTPUT: "${script_address_out}

${cli} transaction build-raw \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --protocol-params-file ../tmp/protocol.json \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${sale_execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/sale/start-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
    --mint="${pointer_asset}" \
    --mint-tx-in-reference="${pointer_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${pointer_pid}" \
    --mint-reference-tx-in-execution-units="${pointer_execution_unts}" \
    --mint-reference-tx-in-redeemer-file ../data/mint/mint-redeemer.json \
    --required-signer-hash ${newm_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee ${total_fee}
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/newm-wallet/payment.skey \
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

tx=$(${cli} transaction txid --tx-file ../tmp/tx.signed)
echo "Tx Hash:" $tx