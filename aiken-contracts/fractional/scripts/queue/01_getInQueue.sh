#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# bundle sale contract
queue_script_path="../../contracts/queue_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${queue_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# collat, buyer, reference
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)

max_bundle_size=$(jq -r '.fields[3].int' ../data/sale/sale-datum.json)
if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Supply A Bundle Amount \033[0m \n";
    exit
fi
if [[ ${1} -eq 0 ]] ; then
    echo -e "\n \033[0;31m Bundle Size Must Be Greater Than Zero \033[0m \n";
    exit
fi
if [[ ${1} -gt ${max_bundle_size} ]] ; then
    echo -e "\n \033[0;31m Bundle Size Must Be Less Than Or Equal To ${max_bundle_size} \033[0m \n";
    exit
fi

bundleSize=${1}
# update bundle size
variable=${bundleSize}; jq --argjson variable "$variable" '.fields[2].int=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json
# update token info
bundle_pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
bundle_tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
variable=${bundle_pid}; jq --arg variable "$variable" '.fields[1].fields[0].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json
variable=${bundle_tkn}; jq --arg variable "$variable" '.fields[1].fields[1].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json

#
buyer_assets=$(python3 -c "import sys; sys.path.append('../py/'); from convertMapToOutput import get_map; get_map($(jq -r '.fields[2].map' ../data/sale/sale-datum.json), ${bundleSize})")

# the pure ada part
pSize=$(jq '.fields[2].map[] | select(.k.bytes == "") | .v.map[].v.int' ../data/sale/sale-datum.json)
payAmt=$((${bundleSize} * ${pSize}))

gas=2000000
if [ -z "$buyer_assets" ]; then
    min_utxo_value=$(${cli} transaction calculate-min-required-utxo \
        --babbage-era \
        --protocol-params-file ../tmp/protocol.json \
        --tx-out-inline-datum-file ../data/queue/queue-datum.json \
        --tx-out="${script_address} + 5000000" | tr -dc '0-9')
    adaPay=$((${min_utxo_value} + ${payAmt} + ${gas}))
    script_address_out="${script_address} + ${adaPay}"
else
    min_utxo_value=$(${cli} transaction calculate-min-required-utxo \
        --babbage-era \
        --protocol-params-file ../tmp/protocol.json \
        --tx-out-inline-datum-file ../data/queue/queue-datum.json \
        --tx-out="${script_address} + 5000000 + ${buyer_assets}" | tr -dc '0-9')
    adaPay=$((${min_utxo_value} + ${payAmt} + ${gas}))
    script_address_out="${script_address} + ${adaPay} + ${buyer_assets}"
fi

echo "Buyer OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
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

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
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
    --signing-key-file ../wallets/buyer-wallet/payment.skey \
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
