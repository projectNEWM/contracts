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

#
buyer_address=$(cat ../wallets/artist-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

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
# TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $artistPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
# artist_utxo=${TXIN::-8}


#
pid=$(jq -r '.fields[1].fields[0].bytes' ../data/sale/sale-datum.json)
tkn=$(jq -r '.fields[1].fields[1].bytes' ../data/sale/sale-datum.json)
total_amt=100000000

TXIN=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .key' ../tmp/script_utxo.json)
artist_tx_in=$TXIN
echo SELLER UTXO ${artist_tx_in}

default_asset="${total_amt} ${pid}.${tkn}"
CURRENT_VALUE=$(jq -r --arg alltxin "" --arg artistPkh "${artist_pkh}" --arg pid "${pid}" --arg tkn "${tkn}" 'to_entries[] | select(.value.value[$pid] // empty | keys[0] == $tkn) | .value.value[$pid][$tkn]' ../tmp/script_utxo.json)
echo $CURRENT_VALUE

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
# update the starting lock time
variable=${bundleSize}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/sale/purchase-redeemer.json > ../data/sale/purchase-redeemer-new.json
mv ../data/sale/purchase-redeemer-new.json ../data/sale/purchase-redeemer.json

bSize=$(jq -r '.fields[1].fields[2].int' ../data/sale/sale-datum.json)
pSize=$(jq -r '.fields[2].fields[2].int' ../data/sale/sale-datum.json)

buyAmt=$((${bundleSize} * ${bSize}))
payAmt=$((${bundleSize} * ${pSize}))
retAmt=$((${CURRENT_VALUE} - ${buyAmt}))

if [[ CURRENT_VALUE -lt buyAmt ]] ; then
    echo "Not Enough Tokens For Bundle Size"
    exit
fi

buyer_asset="${buyAmt} ${pid}.${tkn}"
buyer_utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${script_address} + 5000000 + ${buyer_asset}" | tr -dc '0-9')

script_utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --tx-out="${script_address} + 5000000 + ${default_asset}" | tr -dc '0-9')

returning_asset="${retAmt} ${pid}.${tkn}"

if [[ retAmt -le 0 ]] ; then
    script_address_out="${script_address} + ${script_utxo_value}"
else
    script_address_out="${script_address} + ${script_utxo_value} + ${returning_asset}"
fi
buyer_address_out="${buyer_address} + ${buyer_utxo_value} + ${buyer_asset}"
artist_address_out="${artist_address} + ${payAmt}"
echo "Script OUTPUT: "${script_address_out}
echo "Buyer OUTPUT: "${buyer_address_out}
echo "Seller OUTPUT: "${artist_address_out}
#
# exit
#
# Get tx payer info
echo -e "\033[0;36m Gathering Batcher Bot UTxO Information  \033[0m"
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
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ../tmp/buyer_utxo.json)
buyer_tx_in=${TXIN::-8}

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

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral ${collat_utxo} \
    --tx-in ${buyer_tx_in} \
    --tx-in ${artist_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/sale/purchase-redeemer.json \
    --tx-out="${artist_address_out}" \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json  \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/artist-wallet/payment.skey \
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