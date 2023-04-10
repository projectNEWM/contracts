#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

#
SCRIPT_PATH="../../contracts/reference_contract.plutus"
SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${SCRIPT_PATH} --testnet-magic ${testnet_magic})

#
STARTER_ADDRESS=$(cat ../wallets/starter-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat ../wallets/reference-wallet/payment.addr)
COLLAT_ADDRESS=$(cat ../wallets/collat-wallet/payment.addr)

#
mkdir -p ../tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo -e "\033[1;35m Script Address: \033[0m" 
echo -e "\n \033[1;35m ${SCRIPT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;36m Starter Address: \033[0m" 
echo -e "\n \033[1;36m ${STARTER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${STARTER_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;34m Reference Address: \033[0m" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS} \033[0m \n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;34m Collateral Address: \033[0m" 
echo -e "\n \033[1;34m ${COLLAT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}