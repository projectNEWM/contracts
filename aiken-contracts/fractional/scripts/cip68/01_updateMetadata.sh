#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# cip 68 contract
cip68_script_path="../../contracts/cip68_contract.plutus"
cip68_script_address=$(${cli} address build --payment-script-file ${cip68_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# newm
newm_address=$(cat ../wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

pid=$(cat ../../hashes/policy.hash)
tkn=$(cat ../tmp/reference.token)
# asset to trade
asset="1 ${pid}.${tkn}"
# echo $asset

current_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/cip68/metadata-datum.json \
    --tx-out="${cip68_script_address} + 5000000 + ${asset}" | tr -dc '0-9')

updated_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/cip68/updated-metadata-datum.json \
    --tx-out="${cip68_script_address} + 5000000 + ${asset}" | tr -dc '0-9')

difference=$((${updated_min_utxo} - ${current_min_utxo}))

direction=0
if [ "$difference" -eq "0" ]; then
    echo "Minimum ADA Constant"
    min_utxo=${updated_min_utxo}
    difference=0
elif [ "$difference" -lt "0" ]; then
    positive=$(( -1 * ${difference}))
    echo "Minimum ADA Decreasing by" ${positive}
    direction=1
    difference=$positive
else
    echo "Minimum ADA Increasing by" ${difference}
fi

# assume the min will always be the updated since updated can just be constant
min_utxo=${updated_min_utxo}

# update the difference
variable=${difference}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/cip68/update-redeemer.json > ../data/cip68/update-redeemer-new.json
mv ../data/cip68/update-redeemer-new.json ../data/cip68/update-redeemer.json

# update the direciton, 0 is increase
variable=${direction}; jq --argjson variable "$variable" '.fields[1].int=$variable' ../data/cip68/update-redeemer.json > ../data/cip68/update-redeemer-new.json
    mv ../data/cip68/update-redeemer-new.json ../data/cip68/update-redeemer.json

script_address_out="${cip68_script_address} + ${min_utxo} + ${asset}"
echo "Update OUTPUT: "${script_address_out}
#
# exit
#
# get deleg utxo
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${newm_address} \
    --out-file ../tmp/newm_utxo.json

TXNS=$(jq length ../tmp/newm_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${newm_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/newm_utxo.json)
newm_tx_in=${TXIN::-8}

# get script utxo
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${cip68_script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/script_utxo.json
TXNS=$(jq length ../tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${cip68_script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg policy_id "$pid" --arg name "$tkn" 'to_entries[] | select(.value.value[$policy_id][$name] == 1) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}
echo $script_tx_in
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
collat_tx_in=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

# script reference utxo
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/cip-reference-utxo.signed )
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${newm_address} \
    --tx-in-collateral ${collat_tx_in} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${newm_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/cip68/update-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/cip68/updated-metadata-datum.json \
    --required-signer-hash ${newm_pkh} \
    --required-signer-hash ${collat_pkh} \
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

cp ../data/cip68/updated-metadata-datum.json ../data/cip68/metadata-datum.json

tx=$(cardano-cli transaction txid --tx-file ../tmp/tx.signed)
echo "Tx Hash:" $tx