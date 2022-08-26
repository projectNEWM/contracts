#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)
#
script_path="../nft-locking-contract/nft-locking-contract.plutus"
SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

#
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat wallets/reference-wallet/payment.addr)
COLLAT_ADDRESS=$(cat wallets/collat-wallet/payment.addr)
# MULTISIG_ADDRESS=$(cat wallets/multisig-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo
echo -e "\033[1;35m Script Address:" 
echo -e "\n${SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m NEWM Address:" 
echo -e "\n${SELLER_ADDRESS}\n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Artist Address:" 
echo -e "\n${BUYER_ADDRESS}\n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS}\n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${COLLAT_ADDRESS}\n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

# echo
# echo -e "\033[1;34m Multisig Address: \033[0m" 
# echo -e "\n \033[1;34m ${MULTISIG_ADDRESS} \033[0m \n";
# ${cli} query utxo --address ${MULTISIG_ADDRESS} --testnet-magic ${testnet_magic}
