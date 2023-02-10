#!/bin/bash
set -e

source ../.env

#
mint_path="policy/policy.script"
#
script_path="../nft-locking-contract/nft-locking-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})
# collat, seller, reference
seller_address=$(cat wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
multisig1_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/multisig-wallet/multisig1.vkey)
multisig2_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/multisig-wallet/multisig2.vkey)
multisig3_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/multisig-wallet/multisig3.vkey)

policy_id=$(cat policy/starter.id)
# It'sTheStarterToken4ProjectNewM
token_name=$(cat ../start_info.json | jq -r .starterTkn)
MINT_ASSET="1 ${policy_id}.${token_name}"
#
UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address} ${MINT_ASSET}" | tr -dc '0-9')

script_address_out="${script_address} + 5000000 + ${MINT_ASSET}"

starter_nft_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/worst_case_datum.json | tr -dc '0-9')
    
echo "Starter NFT Min Fee: "${starter_nft_min_utxo}

script_address_out="${script_address} + $starter_nft_min_utxo + ${MINT_ASSET}"
echo "Mint OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    ${network} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/current_datum.json  \
    --mint-script-file policy/policy.script \
    --mint="${MINT_ASSET}" \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${multisig1_pkh} \
    --required-signer-hash ${multisig2_pkh} \
    --required-signer-hash ${multisig3_pkh} \
    --cddl-format \
    ${network})

# ${hwcli} transaction transform --tx-file tmp/tx.draft --out-file tmp/tx.draft

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
#exit
#
# echo -e "\033[0;36m Signing \033[0m"
# ${cli} transaction sign \
#     --signing-key-file wallets/seller-wallet/payment.skey \
#     --tx-body-file tmp/tx.draft \
#     --out-file tmp/tx.signed \
#     ${network}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
   --signing-key-file wallets/multisig-wallet/multisig1.skey \
   --signing-key-file wallets/multisig-wallet/multisig2.skey \
   --signing-key-file wallets/multisig-wallet/multisig3.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    ${network}

# create a witness for seller wallet since we'll re-assemble tx from tx-body with all witnesses
# ${cli} transaction witness --tx-body-file tmp/tx.draft --signing-key-file wallets/seller-wallet/payment.skey ${network} --out-file tmp/seller.witness

#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file tmp/tx.signed
