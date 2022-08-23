#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

#
mint_path="policy/policy.script"
#
script_path="../nft-locking-contract/nft-locking-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
# collat, seller, reference
multisig_address=$(cat wallets/multisig-wallet/payment.addr)
seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)
collat_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)

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
echo "Mint OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${multisig_address} \
    --out-file tmp/multisig_utxo.json

TXNS=$(jq length tmp/multisig_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${multisig_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/multisig_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/multisig_utxo.json)
multisig_tx_in=${TXIN::-8}

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${multisig_address} \
    --tx-in ${multisig_tx_in} \
    --tx-in-script-file wallets/multisig-wallet/payment.script \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/current_datum.json  \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
    --mint-script-file policy/policy.script \
    --mint="${MINT_ASSET}" \
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
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
