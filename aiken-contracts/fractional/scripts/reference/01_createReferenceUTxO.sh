#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# staked smart contract address
script_path="../../contracts/reference_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

# seller info
starter_address=$(cat ../wallets/starter-wallet/payment.addr)

# asset to trade
policy_id=$(jq -r '.starterPid' ../../start_info.json)
token_name=$(jq -r '.starterTkn' ../../start_info.json)
asset="1 ${policy_id}.${token_name}"

min_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/reference/reference-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_value} + ${asset}"
echo "Script OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${starter_address} \
    --out-file ../tmp/starter_utxo.json

# transaction variables
TXNS=$(jq length ../tmp/starter_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${starter_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$policy_id" --arg token_name "$token_name" 'to_entries[] | select((.value.value | length < 2) or .value.value[$policy_id][$token_name] == 1) | .key | . + $alltxin + " --tx-in"' ../tmp/starter_utxo.json)
starter_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${starter_address} \
    --tx-in ${starter_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/reference/reference-datum.json \
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
    --signing-key-file ../wallets/starter-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/referenceable-tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/referenceable-tx.signed

tx=$(cardano-cli transaction txid --tx-file ../tmp/referenceable-tx.signed)
echo "Tx Hash:" $tx