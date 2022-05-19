#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../nft-locking-contract/nft_locking_contract.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
PROFIT_ADDRESS=$(cat wallets/profit-wallet/payment.addr)

echo
${cli} query tip --testnet-magic 1097911063 | jq
#
echo
echo -e "\033[1;35m Script Address: \033[0m" 
echo -e "\n \033[1;35m ${SCRIPT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;36m Seller Address: \033[0m" 
echo -e "\n \033[1;36m ${SELLER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;32m Buyer Address: \033[0m" 
echo -e "\n \033[1;32m ${BUYER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic 1097911063
#
echo
echo -e "\033[1;34m Profit Address: \033[0m" 
echo -e "\n \033[1;34m ${PROFIT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${PROFIT_ADDRESS} --testnet-magic 1097911063
#