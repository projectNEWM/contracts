#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../v2-did-locking-contract/v2-did-locking-contract.plutus"
#
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
delegator_address=$(cat wallets/delegator-wallet/payment.addr)
#
delegator_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/delegator-wallet/payment.vkey)
#
echo "Exit OUTPUT: "${delegator_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${delegator_address} \
    --out-file tmp/delegator_utxo.json

TXNS=$(jq length tmp/delegator_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${delegator_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/delegator_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/delegator_utxo.json)
collateral_tx_in=${CTXIN::-19}
delegator_tx_in=${TXIN::-8}

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
voting_ref_utxo=$(cardano-cli transaction txid --tx-file ../voting-scripts/tmp/vote-tx.signed)
# collat info
collat_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
collat_utxo="87a43ee3889f827356a23a7459ef5f9eaf843880da1996d1b68595fb4171f63c" # in collat wallet

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${delegator_address} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in ${delegator_tx_in} \
    --read-only-tx-in-reference="${voting_ref_utxo}#2" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/exit_redeemer.json \
    --required-signer-hash ${delegator_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic 1097911063)

    # --read-only-tx-in-reference="${voting_ref_utxo}#1" \
IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
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