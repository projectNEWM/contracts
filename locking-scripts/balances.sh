#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../locking-contract/locking_contract.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
SELLER_ADDRESS=$(cat /home/westbam/haskell/newm_seller.addr)
BUYER_ADDRESS=$(cat /home/westbam/haskell/newm_buyer.payment.addr)
PROFIT_ADDRESS=$(cat /home/westbam/haskell/newm_profit.addr)
#SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
#BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
#PROFIT_ADDRESS=$(cat wallets/profit-wallet/payment.addr)

echo
${cli} query tip --testnet-magic 1097911063 | jq
#
echo -e "\033[1;35m"
echo -e " Script Address: " 
echo -e "\n ${SCRIPT_ADDRESS} \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic 1097911063
echo -e "\033[0m"
#
echo
echo -e "\033[1;36m"
echo -e " Seller Address: " 
echo -e "\n ${SELLER_ADDRESS} \n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic 1097911063
echo -e "\033[0m"
#
echo
echo -e "\033[1;32m"
echo -e " Buyer Address: " 
echo -e "\n ${BUYER_ADDRESS} \n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic 1097911063
echo -e "\033[0m"
#
echo
echo -e "\033[1;34m"
echo -e " Profit Address: " 
echo -e "\n ${PROFIT_ADDRESS} \n";
${cli} query utxo --address ${PROFIT_ADDRESS} --testnet-magic 1097911063
echo -e "\033[0m"
#