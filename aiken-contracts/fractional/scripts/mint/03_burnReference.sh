#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# update the starting lock time
variable=0; jq --argjson variable "$variable" '.fields[0].int=$variable' ../data/mint/burn-redeemer.json > ../data/mint/burn-redeemer-new.json
mv ../data/mint/burn-redeemer-new.json ../data/mint/burn-redeemer.json

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

#
newm_address=$(cat ../wallets/newm-wallet/payment.addr)
newm_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/newm-wallet/payment.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

#
artist_address=$(cat ../wallets/artist-wallet/payment.addr)
artist_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/artist-wallet/payment.vkey)

# multisig
keeper1_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/keeper1-wallet/payment.vkey)
keeper2_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/keeper2-wallet/payment.vkey)
keeper3_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/keeper3-wallet/payment.vkey)


# the minting script policy
policy_id=$(cat ../../hashes/policy.hash)
token_name=$(cat ../tmp/reference.token)

echo -e "\033[0;36m Gathering Artist UTxO Information  \033[0m"
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

# echo "Artist UTxO:" $artist_tx_in

BURN_ASSET="-1 ${policy_id}.${token_name}"

# echo $BURN_ASSET


RETURN_ASSET="${retAmt} ${policy_id}.${token_name}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${artist_address} + 5000000" | tr -dc '0-9')

artist_address_out="${artist_address} + ${UTXO_VALUE}"

echo "Artist Return OUTPUT:" ${artist_address_out}
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

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/mint-reference-utxo.signed)
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${newm_address} \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${artist_tx_in} \
    --tx-out="${artist_address_out}" \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${keeper1_pkh} \
    --required-signer-hash ${keeper2_pkh} \
    --required-signer-hash ${keeper3_pkh} \
    --mint="${BURN_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file ../data/mint/burn-redeemer.json \
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
    --signing-key-file ../wallets/collat-wallet/payment.skey \
    --signing-key-file ../wallets/keeper1-wallet/payment.skey \
    --signing-key-file ../wallets/keeper2-wallet/payment.skey \
    --signing-key-file ../wallets/keeper3-wallet/payment.skey \
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