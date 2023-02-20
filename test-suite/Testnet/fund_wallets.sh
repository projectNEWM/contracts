#!/usr/bin/bash
set -e

source .node.env

echo -e "\033[1;35m Funding Test Wallets \033[0m"
echo

cardano-cli query protocol-parameters \
--testnet-magic 42 \
--out-file ${ROOT}/tmp/protocol-parameters.json

spo_addr=$(cat ${ROOT}/addresses/payment1.addr)
cardano-cli query utxo --address ${spo_addr} --testnet-magic 42  --out-file ${ROOT}/tmp/spo_utxo.json

TXNS=$(jq length ${ROOT}/tmp/spo_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${spo_addr}! \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ${ROOT}/tmp/spo_utxo.json)
spo_tx_in=${TXIN::-8}
echo "SPO TxIn: $spo_tx_in"

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --change-address ${spo_addr} \
    --tx-in ${spo_tx_in} \
    --tx-out="$(cat ${ROOT}/addresses/newm.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/artist.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/attacker.addr) + 1000000000" \
    --tx-out-inline-datum-file data/start_tokenized_datum.json  \
    --tx-out="$(cat ${ROOT}/addresses/collat.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/reference.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/multisig1.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/multisig2.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/multisig3.addr) + 1000000000" \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${ROOT}/stake-delegator-keys/payment1.skey \
    --tx-body-file ${ROOT}/tmp/tx.draft \
    --out-file ${ROOT}/tmp/tx-distribution.signed \
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file ${ROOT}/tmp/tx-distribution.signed