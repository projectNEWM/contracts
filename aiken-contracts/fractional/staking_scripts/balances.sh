#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

#
DELEGATOR_ADDRESS=$(cat ../wallets/delegator-wallet/payment.addr)
COLLAT_ADDRESS=$(cat ../wallets/collat-wallet/payment.addr)
#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq
#
echo -e "\033[1;34m Delegator Address: \033[0m" 
echo -e "\n \033[1;34m ${DELEGATOR_ADDRESS} \033[0m \n";
${cli} query utxo --address ${DELEGATOR_ADDRESS} --testnet-magic ${testnet_magic}
#
echo -e "\033[1;34m Collateral Address: \033[0m" 
echo -e "\n \033[1;34m ${COLLAT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}