#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../v2-locking-contract/v2-fractional-locking-contract.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat wallets/reference-wallet/payment.addr)

${cli} query protocol-parameters --testnet-magic 1097911063 --out-file tmp/protocol.json
${cli} query tip --testnet-magic 1097911063 | jq
#
echo
echo -e "\033[1;35m Script Address: \033[0m" 
echo -e "\n \033[1;35m ${SCRIPT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;36m NewM Address: \033[0m" 
echo -e "\n \033[1;36m ${SELLER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;32m Customer Address: \033[0m" 
echo -e "\n \033[1;32m ${BUYER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;34m Reference Address: \033[0m" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS} \033[0m \n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic 1097911063