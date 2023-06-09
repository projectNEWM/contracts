#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat ../data/path_to_socket.sh)
cli=$(cat ../data/path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

contracts_path="../../contracts"

# staking contract
stake_script_path="${contracts_path}/stake_contract.plutus"

# bundle sale contract
sale_script_path="${contracts_path}/sale_contract.plutus"
sale_script_address=$(${cli} address build --payment-script-file ${sale_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

# queue contract
queue_script_path="${contracts_path}/queue_contract.plutus"
queue_script_address=$(${cli} address build --payment-script-file ${queue_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

sale_utxo_path="../tmp/current_sale_utxos.json"
queue_utxo_path="../tmp/current_queue_utxos.json"

echo -e "\033[1;35mQuery Bundle Sale Script Address\033[0m"
${cli} query utxo --address ${sale_script_address} --testnet-magic ${testnet_magic} --out-file ${sale_utxo_path}
#
echo -e "\033[1;35mQuery Queue Script Address\033[0m"
${cli} query utxo --address ${queue_script_address} --testnet-magic ${testnet_magic} --out-file ${queue_utxo_path}

pointer_pid_path="../../hashes/pointer_policy.hash"

python3 -c "from run import run; run('${sale_utxo_path}', '${queue_utxo_path}', '${pointer_pid_path}')"


# a dictionary of queue items to sale items
# { 'queue_utxo': {'sale_utxo':slot_number}}
# a list of queue items in order of slot numbers
# purchase and refund in the list of queue items
# update sale state in order dict and loop all queue items