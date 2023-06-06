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

#
pointer_pid=$(cat ../../hashes/pointer_policy.hash)

pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
total_amt=100000000

default_asset="${total_amt} ${pid}.${tkn}"
pointer_asset="${total_amt} ${pointer_pid}.${tkn}"

utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --tx-out="${script_address} + 5000000 + ${default_asset} + ${pointer_asset}" | tr -dc '0-9')
self_start_fee=1000000
min_ada=$((${utxo_value} + ${self_start_fee}))

script_address_out="${script_address} + ${min_ada} + ${default_asset}"
echo "Artist OUTPUT: "${script_address_out}
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

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${artist_address} \
    --tx-in ${artist_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
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
    --signing-key-file ../wallets/artist-wallet/payment.skey \
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
