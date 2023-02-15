#!/usr/bin/bash
set -e

source .node.env

echo -e "\033[1;35m Creating Test Wallets \033[0m" 

ADDR=newm
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=artist
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=collat
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=reference
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=multisig1
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=multisig2
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=multisig3
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

echo -e "\033[1;35m Funding Test Wallets \033[0m" 

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
echo $spo_tx_in

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --change-address ${spo_addr} \
    --tx-in ${spo_tx_in} \
    --tx-out="$(cat ${ROOT}/addresses/newm.addr) + 1000000000" \
    --tx-out="$(cat ${ROOT}/addresses/artist.addr) + 1000000000" \
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

cardano-cli query utxo --address ${spo_addr} --testnet-magic 42
cardano-cli query utxo --address $(cat ${ROOT}/addresses/newm.addr) --testnet-magic 42


echo -e "\033[1;35m Prepping Contracts \033[0m" 

spo_addr=$(cat ${ROOT}/addresses/payment2.addr)
cardano-cli query utxo --address ${spo_addr} --testnet-magic 42