#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# staking contract
# stake_script_path="../../contracts/stake_contract.plutus"
stake_script_path="stake_test17rq3egpkklttva2h2g8kfrnrm57j3duraqqcgkh8s2uuw4q7ph8gc"

# cip 68 contract
storage_script_path="../../contracts/storage_contract.plutus"
# storage_script_address=$(${cli} address build --payment-script-file ${storage_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})
storage_script_address="addr_test1xzpq3mqglyyrqce7fgh725ra2tp8hv2kssx0kx6ul6gxhskprjsrdd7kke64w5s0vj8x8hfa9zmc86qps3dw0q4eca2q8r9cjd"

# bundle sale contract
sale_script_path="../../contracts/sale_contract.plutus"
# sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})
sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-address ${stake_script_path} --testnet-magic ${testnet_magic})

#
newm_address=$(cat ../wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)
#
receiver_address=$(cat ../wallets/artist-wallet/payment.addr)
receiver_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

# the minting script policy
policy_id=$(cat ../../hashes/pointer_policy.hash)

echo -e "\033[0;36m Gathering NEWM UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${receiver_address} \
    --out-file ../tmp/newm_utxo.json

TXNS=$(jq length ../tmp/newm_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${newm_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/newm_utxo.json)
newm_tx_in=${TXIN::-8}

echo "NEWM UTxO:" $newm_tx_in
first_utxo=$(jq -r 'keys[0]' ../tmp/newm_utxo.json)
string=${first_utxo}
IFS='#' read -ra array <<< "$string"

prefix_555="ca7ab1e0"

pointer_name=$(python3 -c "import sys; sys.path.append('../../lib/py/'); from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_555}')")

echo "Pointer Token Name:" $pointer_name

echo -n $pointer_name > ../tmp/pointer.token


MINT_ASSET="1 ${policy_id}.${pointer_name}"

# UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --tx-out="${newm_address} + 5000000 + ${MINT_ASSET}" | tr -dc '0-9')
# # pointer_address_out="${newm_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
# pointer_address_out="${newm_address} + ${UTXO_VALUE} + ${MINT_ASSET}"

# cost value + incentive + bundle
worst_case_token="9223372036854775807 015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d00000000
+ 9223372036854775807 115d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d00000000
+ 9223372036854775807 215d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d.015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d00000000
"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --tx-out="${sale_script_address} + 5000000 + ${worst_case_token}" | tr -dc '0-9')

self_start_fee=$(jq '.fields[4].fields[2].int' ../data/reference/reference-datum.json)
min_ada=$((${UTXO_VALUE} + ${self_start_fee}))
# fraction_address_out="${sale_script_address} + ${min_ada} + ${FRACTION_ASSET}"

# MINT_ASSET="1 ${policy_id}.${ref_name} + 100000000 ${policy_id}.${frac_name}"

pointer_address_out="${sale_script_address} + ${min_ada} + 1 ${policy_id}.${pointer_name} + 100000000 989b0b633446d55c994ce997634fd5f94bd4e530bfa041448ea75c9c.28343434290198e10f93b990f9eab9fd6d05b2d2a0a08c359f36f123a925c36d"


echo "Pointer Mint OUTPUT:" ${pointer_address_out}
#
# exit
#
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

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/pointer-reference-utxo.signed)
# data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )
data_ref_utxo="8472ae5dd6ff4620ec563f7b00ebd05ba24119274fa69a6e7f015bfaa657ebeb"

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${receiver_address} \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${newm_tx_in} \
    --tx-out="${pointer_address_out}" \
    --tx-out-inline-datum-file ../data/sale/sale-datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${newm_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file ../data/mint/mint-redeemer.json \
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

tx=$(cardano-cli transaction txid --tx-file ../tmp/tx.signed)
echo "Tx Hash:" $tx