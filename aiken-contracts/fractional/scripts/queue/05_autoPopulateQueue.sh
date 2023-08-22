#!/bin/bash
set -e

MAX_NUMBER_OF_TX=50

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

stake_script_path="../../contracts/stake_contract.plutus"
queue_script_path="../../contracts/queue_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${queue_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

max_bundle_size=$(jq -r '.fields[3].int' ../data/sale/sale-datum.json)

number_of_bundles=$(python3 -c "import random; print(random.randint(1, ${max_bundle_size}))")
echo Buying ${number_of_bundles} Bundles
buyer_number=$(python3 -c "import random; print(random.randint(1, 2))")

issuer_path="issuer-wallet"
issuer_address=$(cat ../wallets/${issuer_path}/payment.addr)

# issuer order for
buyer_path="buyer${buyer_number}-wallet"
buyer_address=$(cat ../wallets/${buyer_path}/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/${buyer_path}/payment.vkey)

variable=${buyer_pkh}; jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json

variable=${number_of_bundles}; jq --argjson variable "$variable" '.fields[2].int=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json

# update token info
bundle_pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
bundle_tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
variable=${bundle_pid}; jq --arg variable "$variable" '.fields[1].fields[0].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json
variable=${bundle_tkn}; jq --arg variable "$variable" '.fields[1].fields[1].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json
# point to a sale
pointer_tkn=$(cat ../tmp/pointer.token)
variable=${pointer_tkn}; jq --arg variable "$variable" '.fields[4].bytes=$variable' ../data/queue/queue-datum.json > ../data/queue/queue-datum-new.json
mv ../data/queue/queue-datum-new.json ../data/queue/queue-datum.json

#
buyer_assets=$(python3 -c "import sys; sys.path.append('../py/'); from convertMapToOutput import get_map; get_map($(jq -r '.fields[2].map' ../data/sale/sale-datum.json), ${number_of_bundles})")
# echo $buyer_assets
# the pure ada part if it exists
pSize=$(jq '.fields[2].map[] | select(.k.bytes == "") | .v.map[].v.int' ../data/sale/sale-datum.json)
if [[ -z $pSize ]]; then
  pSize=0
fi
payAmt=$((${number_of_bundles} * ${pSize}))

incentive=" + 1000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"

queue_value="${buyer_assets}${incentive}"

# 1 drip as incentive
# number of bundles of the cost value

# this pays for the fees
pub=$(jq '.fields[4].fields[0].int' ../data/reference/reference-datum.json)
rub=$(jq '.fields[4].fields[1].int' ../data/reference/reference-datum.json)
gas=$((${pub} + ${rub}))
# echo "Maximum Gas Fee:" $gas

min_utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json \
    --tx-out="${script_address} + 5000000 + ${queue_value}" | tr -dc '0-9')

adaPay=$((${min_utxo_value} + ${payAmt} + ${gas}))
script_address_out="${script_address} + ${adaPay} + ${queue_value}"

# lovelace check
if [[ ${1} -eq 0 ]] ; then
    echo -e "\033[0;36m Gathering Issuer UTxO Information  \033[0m"
    ${cli} query utxo \
        --testnet-magic ${testnet_magic} \
        --address ${issuer_address} \
        --out-file ../tmp/issuer_utxo.json
    TXNS=$(jq length ../tmp/issuer_utxo.json)
    if [ "${TXNS}" -eq "0" ]; then
    echo -e "\n \033[0;31m NO UTxOs Found At ${issuer_address} \033[0m \n";
    exit;
    fi
    alltxin=""
    TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/issuer_utxo.json)
    issuer_tx_in=${TXIN::-8}

    data="../tmp/issuer_utxo.json"
    lovelace=$(jq '[.[] | .value."lovelace"] | add' ${data})
    # echo "Lovelace: " ${lovelace}


    assets=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from getAllTokens import concatenate_values; concatenate_values('${data}')")
    # echo Assets $assets
    if [ -z "$assets" ]; then  # check if the result is an empty string
        echo "Result is empty. Exiting."
        # exit 1  # exit the script with an error code
    fi

    bundle_size=$(jq '.fields[2].map[0].v.map[0].v.int' ../data/sale/sale-datum.json)
    bundle_amount=$((${number_of_bundles} * ${bundle_size}))
    # echo "bundle amount" $bundle_amount
    # 
    assets=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from subtract_value_string import subtract_value; subtract_value('${assets}', ${bundle_amount}, 1000000)")
    # echo otehr assets $assets
    counter=0
else
    lovelace=${1}
    assets=${2}
    issuer_tx_in=${3}
    counter=${4}

    if [[ ${counter} -gt ${MAX_NUMBER_OF_TX} ]] ; then
        exit
    fi

    echo $lovelace
    echo $issuer_tx_in
    echo "HERE IS THE ASSETS" $assets

    bundle_size=$(jq '.fields[2].map[0].v.map[0].v.int' ../data/sale/sale-datum.json)
    bundle_amount=$((${number_of_bundles} * ${bundle_size}))
    assets=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from subtract_value_string import subtract_value; subtract_value('${assets}', ${bundle_amount}, 1000000)")
    echo "HERE IS THE ASSETS" $assets
    # exit
fi

adaPay=$((${lovelace} - ${adaPay}))

issuer_address_out="${issuer_address} + ${adaPay} + ${assets}"

echo "Script OUTPUT: "${script_address_out}
echo "Issuer OUTPUT: "${issuer_address_out}
#
# exit
#
echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${issuer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
    --tx-out="${issuer_address_out}" \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 1 --tx-out-count 2 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

lovelace_return=$((${adaPay} - ${fee}))
issuer_address_out="${issuer_address} + ${lovelace_return} + ${assets}"


${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${issuer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/queue/queue-datum.json  \
    --tx-out="${issuer_address_out}" \
    --fee ${fee}
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/${issuer_path}/payment.skey \
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

python3 -c "import time, random; time.sleep(random.uniform(0, 10))"
increment=$((${counter} + 1))
./05_autoPopulateQueue.sh ${lovelace_return} "${assets}" "${tx}#1" ${increment}