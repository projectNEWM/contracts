#!/usr/bin/bash
set -e

source .node.env

echo -e "\033[1;35m Creating Test Wallets \033[0m" 

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

cp test_keys/* ${ROOT}/addresses/

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

echo
echo -e "\033[1;35m Prepping Contracts \033[0m"
echo

nft_lock_script_path="contracts/nft-locking-contract.plutus"
nft_mint_script_path="contracts/nft-minting-contract.plutus"

lock_script_path="contracts/locking-contract.plutus"
mint_script_path="contracts/minting-contract.plutus"

# save the script addresses into the address folder
echo -n $(${cli} address build --payment-script-file ${nft_lock_script_path} ${network}) > ${ROOT}/addresses/nftLock.addr
echo -n $(${cli} address build --payment-script-file ${nft_mint_script_path} ${network}) > ${ROOT}/addresses/nftMint.addr
echo -n $(${cli} address build --payment-script-file ${lock_script_path} ${network}) > ${ROOT}/addresses/ftLock.addr
echo -n $(${cli} address build --payment-script-file ${mint_script_path} ${network}) > ${ROOT}/addresses/ftMint.addr

spo_addr=$(cat ${ROOT}/addresses/payment2.addr)
reference_address=$(cat ${ROOT}/addresses/reference.addr)
cardano-cli query utxo --address ${spo_addr} --testnet-magic 42 --out-file ${ROOT}/tmp/spo_utxo.json

TXNS=$(jq length ${ROOT}/tmp/spo_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${spo_addr}! \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ${ROOT}/tmp/spo_utxo.json)
spo_tx_in=${TXIN::-8}
echo "SPO TxIn: $spo_tx_in"

echo -e "\033[0;36m Calculating Reference ADA \033[0m"
lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "NFT Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "NFT Minting Min Fee" ${mint_min_utxo}

nft_mint_value=$mint_min_utxo
nft_lock_value=$lock_min_utxo
nft_lock_script_reference_utxo="${reference_address} + ${nft_lock_value}"
nft_mint_script_reference_utxo="${reference_address} + ${nft_mint_value}"

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "FT Locking Min Fee" ${lock_min_utxo}

mint_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --tx-out-reference-script-file ${mint_script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "FT Minting Min Fee" ${mint_min_utxo}

mint_value=$mint_min_utxo
lock_value=$lock_min_utxo
lock_script_reference_utxo="${reference_address} + ${lock_value}"
mint_script_reference_utxo="${reference_address} + ${mint_value}"

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_spo_lovelace=$(jq '[.. | objects | .lovelace] | add' ${ROOT}/tmp/spo_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --tx-in ${spo_tx_in} \
    --tx-out="${spo_addr} + ${starting_spo_lovelace}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --fee 900000
FEE=$(${cli} transaction calculate-min-fee --tx-body-file ${ROOT}/tmp/tx.draft ${network} --protocol-params-file ${ROOT}/tmp/protocol-parameters.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
firstReturn=$((${starting_spo_lovelace} - ${nft_mint_value} - ${nft_lock_value} - ${fee}))
# echo $firstReturn
# exit
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --tx-in ${spo_tx_in} \
    --tx-out="${spo_addr} + ${firstReturn}" \
    --tx-out="${nft_lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_lock_script_path} \
    --tx-out="${nft_mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${nft_mint_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${ROOT}/stake-delegator-keys/payment2.skey \
    --tx-body-file ${ROOT}/tmp/tx.draft \
    --out-file ${ROOT}/tmp/tx-1.signed \
    ${network}

nextUTxO=$(${cli} transaction txid --tx-body-file ${ROOT}/tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

#
# exit
#
echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${spo_addr} + ${firstReturn}" \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${mint_script_reference_utxo}" \
    --tx-out-reference-script-file ${mint_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ${ROOT}/tmp/tx.draft ${network} --protocol-params-file ${ROOT}/tmp/protocol-parameters.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
secondReturn=$((${firstReturn} - ${mint_value} - ${lock_value} - ${fee}))


${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${spo_addr} + ${secondReturn}" \
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
    --signing-key-file ${ROOT}/stake-delegator-keys/payment2.skey \
    --tx-body-file ${ROOT}/tmp/tx.draft \
    --out-file ${ROOT}/tmp/tx-2.signed \
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file ${ROOT}/tmp/tx-1.signed

${cli} transaction submit \
    ${network} \
    --tx-file ${ROOT}/tmp/tx-2.signed

cp ${ROOT}/tmp/tx-1.signed ${ROOT}/tmp/tx-tokenized-utxo.signed
cp ${ROOT}/tmp/tx-2.signed ${ROOT}/tmp/tx-fractions-utxo.signed

echo
echo -e "\033[1;35m Creating Catalog Token \033[0m"
echo

spo_addr=$(cat ${ROOT}/addresses/payment3.addr)
script_address=$(${cli} address build --payment-script-file ${nft_lock_script_path} ${network})
cardano-cli query utxo --address ${spo_addr} --testnet-magic 42 --out-file ${ROOT}/tmp/spo_utxo.json

TXNS=$(jq length ${ROOT}/tmp/spo_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${spo_addr}! \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ${ROOT}/tmp/spo_utxo.json)
spo_tx_in=${TXIN::-8}
echo "SPO TxIn: $spo_tx_in"

# Get multisig
multisig1_pkh=$(${cli} address key-hash --payment-verification-key-file ${ROOT}/addresses/multisig1.vkey)
multisig2_pkh=$(${cli} address key-hash --payment-verification-key-file ${ROOT}/addresses/multisig2.vkey)
multisig3_pkh=$(${cli} address key-hash --payment-verification-key-file ${ROOT}/addresses/multisig3.vkey)

policy_id=$(cat policy/starter.id)
# It'sTheStarterToken4ProjectNewM
token_name=$(cat ../start_info.json | jq -r .starterTkn)
mint_asset="1 ${policy_id}.${token_name}"

echo "Starter Token: ${mint_asset}"

script_address_out="${script_address} + 5000000 + ${mint_asset}"

starter_nft_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json\
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/worst_case_tokenized_datum.json | tr -dc '0-9')
    
echo "Starter NFT Min ADA: "${starter_nft_min_utxo}

script_address_out="${script_address} + $starter_nft_min_utxo + ${mint_asset}"

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --change-address ${spo_addr} \
    --tx-in ${spo_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/current_tokenized_datum.json  \
    --mint-script-file policy/policy.script \
    --mint="${mint_asset}" \
    --required-signer-hash ${multisig1_pkh} \
    --required-signer-hash ${multisig2_pkh} \
    --required-signer-hash ${multisig3_pkh} \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
#exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${ROOT}/stake-delegator-keys/payment3.skey \
    --signing-key-file ${ROOT}/addresses/multisig1.skey \
    --signing-key-file ${ROOT}/addresses/multisig2.skey \
    --signing-key-file ${ROOT}/addresses/multisig3.skey \
    --tx-body-file ${ROOT}/tmp/tx.draft \
    --out-file ${ROOT}/tmp/tx.signed \
    ${network}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file ${ROOT}/tmp/tx.signed