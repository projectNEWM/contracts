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
queue_script_address2=$(${cli} address build --payment-script-file ${queue_script_path} --testnet-magic ${testnet_magic})

# collat, artist, reference
#
newm_address=$(cat ../wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

batcher_address=$(cat ../wallets/batcher-wallet/payment.addr)
batcher_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/batcher-wallet/payment.vkey)


artist_address=$(cat ../wallets/artist-wallet/payment.addr)
artist_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

#
which_buyer="buyer1"
buyer_address=$(cat ../wallets/${which_buyer}-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/${which_buyer}-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

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
# exit

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

cp ../tmp/sale_script_utxo.json ../tmp/script_utxo.json
returning_asset=$(python3 ../py/token_string.py)

# exit

pointer_pid=$(cat ../../hashes/pointer_policy.hash)
pointer_tkn=$(cat ../tmp/pointer.token)
pointer_asset="1 ${pointer_pid}.${pointer_tkn}"

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
if [[ -z $pSize ]]; then
  pSize=0
fi
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

echo Buyer Bundle: $bundle_value
echo Sale Return: $returning_asset

sale_profit_amount=$(jq '[.[] | .value["015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d"]["43757272656e6379"]] | add' ../tmp/sale_script_utxo.json)
queue_payment_amount=$(jq '[.[] | .value["015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d"]["43757272656e6379"]] | add' ../tmp/queue_script_utxo.json)
if [ -z "$sale_profit_amount" ]; then
    profit="$((${queue_payment_amount})) 015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.43757272656e6379"
else
    profit="$((${sale_profit_amount} + ${queue_payment_amount})) 015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.43757272656e6379"
fi
echo Current Profit: $profit

echo Batcher Incentive: $batcher_starting_incentive
incentive="$((1000000 + ${batcher_starting_incentive})) 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"
return_incentive="1000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"
token_name="5ca1ab1e000affab1e000ca11ab1e0005e77ab1e"
batcher_policy_id=$(cat ../../hashes/batcher.hash)
batcher_token="1 ${batcher_policy_id}.${token_name}"
batcher_address_out="${batcher_address} + ${batcher_starting_lovelace} + ${incentive} + ${batcher_token}"



# queue contract return
# the cost value is ada
queue_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/queue_script_utxo.json)
sale_utxo_value=$(jq -r '.[].value.lovelace' ../tmp/sale_script_utxo.json)
queue_ada_return=$((${queue_utxo_value}))
sale_ada_return=$((${sale_utxo_value}))

if [[ retAmt -le 0 ]] ; then
        # echo "THIS CLEANS THE SALE OUT"
        sale_script_address_out="${sale_script_address} + ${sale_ada_return} + ${pointer_asset} + ${profit}"
        queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value} + ${return_incentive}"
        # echo $sale_script_address_out
        # exit
    else
        sale_script_address_out="${sale_script_address} + ${sale_ada_return} + ${returning_asset} + ${pointer_asset} + ${profit}"
        queue_script_address_out="${queue_script_address} + ${queue_ada_return} + ${bundle_value} + ${return_incentive}"
    fi

echo -e "\nBatcher OUTPUT: "${batcher_address_out}
echo -e "\nSale Script OUTPUT: "${sale_script_address_out}
echo -e "\nQueue Script OUTPUT: "${queue_script_address_out}
#
# exit
#

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


execution_unts="(0, 0)"

echo -e "\033[0;36m Building Tx \033[0m"
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
    --spending-reference-tx-in-execution-units="${execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
    --tx-in ${queue_tx_in} \
    --spending-tx-in-reference="${queue_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-execution-units="${execution_unts}" \
    --spending-reference-tx-in-redeemer-file ../data/queue/purchase-redeemer.json \
    --tx-out="${batcher_address_out}" \
    --tx-out="${sale_script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
    --tx-out="${queue_script_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
    --required-signer-hash ${batcher_pkh} \
    --required-signer-hash ${collat_pkh} \
    --fee 0

python3 -c "import sys, json; sys.path.append('../py/'); from tx_simulation import from_file; exe_units=from_file('../tmp/tx.draft', False);print(json.dumps(exe_units))" > ../data/exe_units.json

cat ../data/exe_units.json

cpu=$(jq -r '.[0].cpu' ../data/exe_units.json)
mem=$(jq -r '.[0].mem' ../data/exe_units.json)

sale_execution_unts="(${cpu}, ${mem})"
sale_computation_fee=$(echo "0.0000721*${cpu} + 0.0577*${mem}" | bc)
sale_computation_fee_int=$(printf "%.0f" "$sale_computation_fee")

cpu=$(jq -r '.[1].cpu' ../data/exe_units.json)
mem=$(jq -r '.[1].mem' ../data/exe_units.json)

queue_execution_unts="(${cpu}, ${mem})"
queue_computation_fee=$(echo "0.0000721*${cpu} + 0.0577*${mem}" | bc)
queue_computation_fee_int=$(printf "%.0f" "$queue_computation_fee")
#
# exit
#
FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 3 --tx-out-count 3 --witness-count 2)
fee=$(echo $FEE | rev | cut -c 9- | rev)

total_fee=$((${fee} + ${sale_computation_fee_int} + ${queue_computation_fee_int}))
echo Tx Fee: $total_fee
change_value=$((${queue_ada_return} - ${total_fee}))
queue_script_address_out="${queue_script_address} + ${change_value} + ${bundle_value}  + ${return_incentive}"
echo "Without Fee: Queue Script OUTPUT: "${queue_script_address_out}

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
