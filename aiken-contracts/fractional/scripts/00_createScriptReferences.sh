#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat ./data/path_to_socket.sh)
cli=$(cat ./data/path_to_cli.sh)
testnet_magic=$(cat ./data/testnet.magic)

mkdir -p ./tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json

# contract path
cip68_script_path="../contracts/cip68_contract.plutus"
stake_script_path="../contracts/stake_contract.plutus"
refer_script_path="../contracts/reference_contract.plutus"
mint_script_path="../contracts/mint_contract.plutus"
sale_script_path="../contracts/sale_contract.plutus"
queue_script_path="../contracts/queue_contract.plutus"
pointer_script_path="../contracts/pointer_contract.plutus"

# Addresses
reference_address=$(cat ./wallets/reference-wallet/payment.addr)
script_reference_address=$(cat ./wallets/reference-wallet/payment.addr)

cip68_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${cip68_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

cip68_value=$((${cip68_min_utxo}))
cip68_script_reference_utxo="${script_reference_address} + ${cip68_value}"

stake_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

stake_value=$((${stake_min_utxo}))
stake_script_reference_utxo="${script_reference_address} + ${stake_value}"

ref_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${refer_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

ref_value=$((${ref_min_utxo}))
ref_script_reference_utxo="${script_reference_address} + ${ref_value}"

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${mint_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

mint_value=$((${mint_min_utxo}))
mint_script_reference_utxo="${script_reference_address} + ${mint_value}"

sale_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${sale_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

sale_value=$((${sale_min_utxo}))
sale_script_reference_utxo="${script_reference_address} + ${sale_value}"

queue_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${queue_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

queue_value=$((${queue_min_utxo}))
queue_script_reference_utxo="${script_reference_address} + ${queue_value}"

pointer_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --tx-out-reference-script-file ${pointer_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

pointer_value=$((${pointer_min_utxo}))
pointer_script_reference_utxo="${script_reference_address} + ${pointer_value}"

echo -e "\nCreating CIP68 Script:\n" ${cip68_script_reference_utxo}
echo -e "\nCreating Stake Script:\n" ${stake_script_reference_utxo}
echo -e "\nCreating Refer Script:\n" ${ref_script_reference_utxo}
echo -e "\nCreating Mint Script:\n" ${mint_script_reference_utxo}
echo -e "\nCreating Sale Script:\n" ${sale_script_reference_utxo}
echo -e "\nCreating Queue Script:\n" ${queue_script_reference_utxo}
echo -e "\nCreating Pointer Script:\n" ${pointer_script_reference_utxo}
#
# exit
#
echo -e "\033[0;35m\nGathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${reference_address} \
    --out-file ./tmp/reference_utxo.json

TXNS=$(jq length ./tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ./tmp/reference_utxo.json)
ref_tx_in=${TXIN::-8}
#
# exit
#
###############################################################################
# chain second set of reference scripts to the first
echo -e "\033[0;33m\nStart Building Tx Chain \033[0m"
echo -e "\033[0;36m Building Tx \033[0m"
starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' ./tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${cip68_script_reference_utxo}" \
    --tx-out-reference-script-file ${cip68_script_path} \
    --fee 900000

FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
firstReturn=$((${starting_reference_lovelace} - ${cip68_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${cip68_script_reference_utxo}" \
    --tx-out-reference-script-file ${cip68_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
secondReturn=$((${firstReturn} - ${stake_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --fee ${fee}
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-2.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "Second in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${refer_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

thirdReturn=$((${secondReturn} - ${ref_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${refer_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-3.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "Third in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

fourthReturn=$((${thirdReturn} - ${mint_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fourthReturn}" \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-4.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "Fourth in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fourthReturn}" \
    --tx-out="${sale_script_reference_utxo}" \
    --tx-out-reference-script-file ${sale_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

fifthReturn=$((${fourthReturn} - ${sale_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fifthReturn}" \
    --tx-out="${sale_script_reference_utxo}" \
    --tx-out-reference-script-file ${sale_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-5.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "Fifth in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fourthReturn}" \
    --tx-out="${queue_script_reference_utxo}" \
    --tx-out-reference-script-file ${queue_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

sixthReturn=$((${fifthReturn} - ${queue_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${sixthReturn}" \
    --tx-out="${queue_script_reference_utxo}" \
    --tx-out-reference-script-file ${queue_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-6.signed \
    --testnet-magic ${testnet_magic}

###############################################################################

nextUTxO=$(${cli} transaction txid --tx-body-file ./tmp/tx.draft)
echo "Sixth in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${sixthReturn}" \
    --tx-out="${pointer_script_reference_utxo}" \
    --tx-out-reference-script-file ${pointer_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ./tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ./tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

seventhReturn=$((${sixthReturn} - ${pointer_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ./tmp/protocol.json \
    --out-file ./tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${seventhReturn}" \
    --tx-out="${pointer_script_reference_utxo}" \
    --tx-out-reference-script-file ${pointer_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ./wallets/reference-wallet/payment.skey \
    --tx-body-file ./tmp/tx.draft \
    --out-file ./tmp/tx-7.signed \
    --testnet-magic ${testnet_magic}

#
# exit
#
echo -e "\033[0;34m\nSubmitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-1.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-2.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-3.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-4.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-5.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-6.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ./tmp/tx-7.signed
#

cp ./tmp/tx-1.signed ./tmp/cip-reference-utxo.signed
cp ./tmp/tx-2.signed ./tmp/stake-reference-utxo.signed
cp ./tmp/tx-3.signed ./tmp/data-reference-utxo.signed
cp ./tmp/tx-4.signed ./tmp/mint-reference-utxo.signed
cp ./tmp/tx-5.signed ./tmp/sale-reference-utxo.signed
cp ./tmp/tx-6.signed ./tmp/queue-reference-utxo.signed
cp ./tmp/tx-7.signed ./tmp/pointer-reference-utxo.signed

echo -e "\033[0;32m\nDone! \033[0m"
