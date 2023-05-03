#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat ./data/path_to_socket.sh)
cli=$(cat ./data/path_to_cli.sh)
testnet_magic=$(cat ./data/testnet.magic)

# staking contract
stake_script_path="../contracts/stake_contract.plutus"

# cip 68 contract
cip68_script_path="../contracts/cip68_contract.plutus"
cip68_script_address=$(${cli} address build --payment-script-file ${cip68_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# bundle sale contract
sale_script_path="../contracts/sale_contract.plutus"
sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# bundle sale contract
queue_script_path="../contracts/queue_contract.plutus"
queue_script_address=$(${cli} address build --payment-script-file ${queue_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# staked smart contract address
ref_script_path="../contracts/reference_contract.plutus"
ref_script_address=$(${cli} address build --payment-script-file ${ref_script_path} --testnet-magic ${testnet_magic})

${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ./tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq
#
echo -e "\033[1;35m Reference Script Address: \033[0m"
echo -e "\n \033[1;35m ${ref_script_address} \033[0m \n";
${cli} query utxo --address ${ref_script_address} --testnet-magic ${testnet_magic}
#
echo -e "\033[1;35m CIP68 Script Address: \033[0m" 
echo -e "\n \033[1;35m ${cip68_script_address} \033[0m \n";
${cli} query utxo --address ${cip68_script_address} --testnet-magic ${testnet_magic}
#
echo -e "\033[1;35m Bundle Sale Script Address: \033[0m" 
echo -e "\n \033[1;35m ${sale_script_address} \033[0m \n";
${cli} query utxo --address ${sale_script_address} --testnet-magic ${testnet_magic}
#
echo -e "\033[1;35m Queue Script Address: \033[0m" 
echo -e "\n \033[1;35m ${queue_script_address} \033[0m \n";
${cli} query utxo --address ${queue_script_address} --testnet-magic ${testnet_magic}

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
        ${cli} query utxo --address ${addr} --testnet-magic ${testnet_magic} --out-file ./tmp/"${addr}.json"
        echo -e "\033[0m"
    fi
done


# addr="addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp"
# ${cli} query utxo --address ${addr} --testnet-magic ${testnet_magic}