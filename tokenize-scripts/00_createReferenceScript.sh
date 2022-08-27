#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)
mkdir -p tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

nft_lock_script_path="../nft-locking-contract/nft-locking-contract.plutus"
nft_mint_script_path="../nft-minting-contract/nft-minting-contract.plutus"

lock_script_path="../locking-contract/locking-contract.plutus"
mint_script_path="../minting-contract/minting-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)
seller_address=$(cat wallets/seller-wallet/payment.addr)

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "NFT Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "NFT Minting Min Fee" ${mint_min_utxo}

echo
nft_mint_value=$mint_min_utxo
nft_lock_value=$lock_min_utxo
nft_lock_script_reference_utxo="${reference_address} + ${nft_lock_value}"
nft_mint_script_reference_utxo="${reference_address} + ${nft_mint_value}"

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "FT Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${mint_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "FT Minting Min Fee" ${mint_min_utxo}

mint_value=$mint_min_utxo
lock_value=$lock_min_utxo
lock_script_reference_utxo="${reference_address} + ${lock_value}"
mint_script_reference_utxo="${reference_address} + ${mint_value}"

echo -e "\nCreating NFT Locking Reference:\n" ${nft_lock_script_reference_utxo}
echo -e "\nCreating NFT Minting Reference:\n" ${nft_mint_script_reference_utxo}
echo -e "\nCreating Locking Reference:\n" ${lock_script_reference_utxo}
echo -e "\nCreating Minting Reference:\n" ${mint_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
HEXTXIN=${TXIN::-8}
# echo $HEXTXIN
# exit

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_seller_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/seller_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${seller_address} + ${starting_seller_lovelace}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --fee 900000
FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
firstReturn=$((${starting_seller_lovelace} - ${nft_mint_value} - ${nft_lock_value} - ${fee}))
# echo $firstReturn
# exit
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${seller_address} + ${firstReturn}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}

nextUTxO=$(cardano-cli transaction txid --tx-body-file tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

#
# exit
#
echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${seller_address} + ${firstReturn}" \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee 900000

FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
secondReturn=$((${firstReturn} - ${mint_value} - ${lock_value} - ${fee}))


${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${seller_address} + ${secondReturn}" \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee ${fee}
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-2.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-1.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-2.signed

cp tmp/tx-1.signed tmp/tx-reference-utxo.signed
cp tmp/tx-2.signed ../fractionalize-scripts/tmp/tx-reference-utxo.signed