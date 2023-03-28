#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat ./data/path_to_socket.sh)
cli=$(cat ./data/path_to_cli.sh)
testnet_magic=$(cat ./data/testnet.magic)

script_path="../cip68.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq
#
echo -e "\033[1;35m CIP68 Script Address: \033[0m" 
echo -e "\n \033[1;35m ${script_address} \033[0m \n";
${cli} query utxo --address ${script_address} --testnet-magic ${testnet_magic}

# Loop through each -wallet folder
for wallet_folder in wallets/*-wallet; do
    # Check if payment.addr file exists in the folder
    if [ -f "${wallet_folder}/payment.addr" ]; then
        addr=$(cat ${wallet_folder}/payment.addr)
        echo
        echo -e "\033[1;34m $wallet_folder $addr \033[0m"
        echo -e "\033[1;33m"
        # Run the cardano-cli command with the reference address and testnet magic
        ${cli} query utxo --address ${addr} --testnet-magic ${testnet_magic}
        echo -e "\033[0m"
    fi
done