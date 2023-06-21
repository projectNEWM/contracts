#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staking contract
stake_script_path="../../contracts/stake_contract.plutus"

# bundle sale contract
order_book_script_path="../../contracts/order_book_contract.plutus"
script_address=$(${cli} address build --payment-script-file ${order_book_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# collat, artist, reference
buyer="buyer2"
buyer_address=$(cat ../wallets/${buyer}-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/${buyer}-wallet/payment.vkey)

if [ "$buyer" = "buyer1" ]; then
    datum_path="../data/order_book/order-book-datum1.json"
elif [ "$buyer" = "buyer2" ]; then
    datum_path="../data/order_book/order-book-datum2.json"
else
    exit
fi

# update owner data
# jq \
# --arg pkh "$buyer_pkh" \
# '.fields[0].fields[0].bytes=$pkh' \
# ../data/order_book/order-book-datum.json | sponge ../data/order_book/order-book-datum.json

#
pid=$(jq -r '.fields[1].fields[0].bytes' ${datum_path})
tkn=$(jq -r '.fields[1].fields[1].bytes' ${datum_path})
total_amt=2000000
want_asset="${total_amt} ${pid}.${tkn}"

ipid=$(jq -r '.fields[3].fields[0].bytes' ${datum_path})
itkn=$(jq -r '.fields[3].fields[1].bytes' ${datum_path})
iamt=$(jq -r '.fields[3].fields[2].int' ${datum_path})
incentive_asset="${iamt} ${ipid}.${itkn}"

utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ${datum_path} \
    --tx-out="${script_address} + 5000000 + ${want_asset} + ${incentive_asset}" | tr -dc '0-9')

self_start_fee=2000000
min_ada=$((${utxo_value} + ${self_start_fee}))

script_address_out="${script_address} + ${min_ada} + ${want_asset} + ${incentive_asset}"
echo "Order OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
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
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ${datum_path}  \
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
    --signing-key-file ../wallets/${buyer}-wallet/payment.skey \
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
