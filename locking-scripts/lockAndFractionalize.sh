#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../locking-contract/locking_contract.plutus"
mint_path="../minting-contract/minting_contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
#
buyer_address=$(cat /home/westbam/haskell/newm_buyer.payment.addr)
buyer_pkh=$(cardano-cli address key-hash --payment-verification-key-file /home/westbam/haskell/newm_buyer.payment.vkey)
#
seller_address=$(cat /home/westbam/haskell/newm_seller.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file /home/westbam/haskell/newm_seller.vkey)
#
policy_id=$(cat ../minting-contract/policy.id)
#
SC_ASSET="1 769c4c6e9bc3ba5406b9b89fb7beb6819e638ff2e2de63f008d5bcff.4e45574d31"
MINT_ASSET="100 ${policy_id}.4e45574d31"
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${buyer_address} ${SC_ASSET}" | tr -dc '0-9')
#
script_address_out="${script_address} + 5000000 + ${SC_ASSET}"
buyer_address_out="${buyer_address} + ${UTXO_VALUE} + ${MINT_ASSET}"
echo "Script OUTPUT: "${script_address_out}
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
#echo "TXIN=\"$TXIN\""
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/buyer_utxo.json)
#echo "CTXIN=\"$CTXIN\""
collateral_tx_in=${CTXIN::-19}
#echo "colateral_tx_in=\"$collateral_tx_in\""
buyer_tx_in=${TXIN::-8}
#echo "buyer_tx_in=\"$buyer_tx_in\""

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
echo "script_tx_in=\"$script_tx_in\""

# exit
echo -e "\033[0;36m Building Tx \033[0m"

echo "cardano-cli transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral 8176145e8d4f4e2f42b7d2a811722fcd25403155892028c351f83431366c1c32#0 \
    --tx-in 6a0ae4a19d4251a6900e0d7009719c00b72dbff988c8c73ffb8a300eb410ec63#0 \
    --tx-in 2904acf79d2206dd138971fc05f10f690ad4a102ae07dfc18051a636043544ae#1 \
    --tx-in-datum-file data/datum.json \
    --tx-in-redeemer-file data/lock_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-datum-embed-file data/datum.json \
    --required-signer-hash ${buyer_pkh} \
    --required-signer-hash ${seller_pkh} \
    --tx-in-script-file ${script_path} \
    --mint="${MINT_ASSET}" \
    --mint-redeemer-file data/datum.json \
    --mint-script-file ${mint_path} \
    --testnet-magic 1097911063"

FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in-collateral 8176145e8d4f4e2f42b7d2a811722fcd25403155892028c351f83431366c1c32#0 \
    --tx-in 6a0ae4a19d4251a6900e0d7009719c00b72dbff988c8c73ffb8a300eb410ec63#0 \
    --tx-in 2904acf79d2206dd138971fc05f10f690ad4a102ae07dfc18051a636043544ae#1 \
    --tx-in-datum-file data/datum.json \
    --tx-in-redeemer-file data/lock_redeemer.json \
    --tx-out="${buyer_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-datum-embed-file data/datum.json \
    --required-signer-hash ${buyer_pkh} \
    --required-signer-hash ${seller_pkh} \
    --tx-in-script-file ${script_path} \
    --mint="${MINT_ASSET}" \
    --mint-redeemer-file data/datum.json \
    --mint-script-file ${mint_path} \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file /home/westbam/haskell/newm_buyer.payment.skey \
    --signing-key-file /home/westbam/haskell/newm_seller.skey \
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
