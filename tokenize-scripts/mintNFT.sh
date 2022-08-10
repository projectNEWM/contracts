#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../nft-locking-contract/nft-locking-contract.plutus"
mint_path="../nft-minting-contract/nft-minting-contract.plutus"


ft_script_path="../locking-contract/locking-contract.plutus"
ft_script_address=$(${cli} address build --payment-script-file ${ft_script_path} --testnet-magic 1097911063)


script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
#
buyer_address=$(cat wallets/buyer-wallet/payment.addr)
buyer_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)
#
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
#
deleg_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)

#
policy_id=$(cat ../nft-minting-contract/policy.id)
#
name=$(echo -n "NewM_0" | xxd -ps)
MINT_ASSET="1 ${policy_id}.${name}"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${buyer_address} ${MINT_ASSET}" | tr -dc '0-9')
#
script_address_out="${script_address} + 5000000 + 1 982f93a0efde8edd0e9af400da083e91d98e1d5b4a77a07938a4de4f.74686973697361766572796c6f6e67737472696e67666f7274657374696e3130"
ft_script_address_out="${ft_script_address} + 5000000"
buyer_address_out="${buyer_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
echo "Script OUTPUT: "${script_address_out}
echo "Script OUTPUT: "${ft_script_address_out}
echo "Mint OUTPUT: "${buyer_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${buyer_address} \
    --out-file tmp/buyer_utxo.json

TXNS=$(jq length tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/buyer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/buyer_utxo.json)
collateral_tx_in=${CTXIN::-19}
buyer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json

# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(cardano-cli transaction txid --tx-file tmp/tx-reference-utxo.signed)
# collat info
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
collat_utxo="87a43ee3889f827356a23a7459ef5f9eaf843880da1996d1b68595fb4171f63c" # in collat wallet

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in ${buyer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/mint_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/next_datum.json \
    --required-signer-hash ${deleg_pkh} \
    --required-signer-hash ${collat_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#2" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file data/current_datum.json \
    --testnet-magic 1097911063)

    # --tx-out="${ft_script_address_out}" \
    # --tx-out-inline-datum-file ../fractionalize-scripts/data/datum.json  \
IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/buyer-wallet/payment.skey \
    --signing-key-file wallets/delegator-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed