#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# stake address
script_path="../../contracts/swap_contract.plutus"
stake_address=$(cardano-cli stake-address build --stake-script-file ${script_path} --testnet-magic ${testnet_magic})
echo stake_address: $stake_address

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# reward fee payer
payee_address=$(cat ../wallets/delegator-wallet/payment.addr)

# rewarder
reward_address=$(cat ../wallets/reward-wallet/payment.addr)

# find rewards
rewardBalance=$(${cli} query stake-address-info \
    --testnet-magic ${testnet_magic} \
    --address ${stake_address} | jq -r ".[0].rewardAccountBalance")
echo rewardBalance: $rewardBalance

if [ "$rewardBalance" = "null" ]; then
   echo -e "\n \033[0;31m No Rewards Found At ${stake_address} \033[0m \n";
   exit;
fi

withdrawalString="${stake_address}+${rewardBalance}"
echo $withdrawalString
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${payee_address} \
    --out-file ../tmp/payee_utxo.json

TXNS=$(jq length ../tmp/payee_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${payee_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ../tmp/payee_utxo.json)
seller_tx_in=${TXIN::-8}

# collat info
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file ../tmp/collat_utxo.json

TXNS=$(jq length ../tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/stake-reference-utxo.signed)
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${payee_address} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${seller_tx_in} \
    --withdrawal ${withdrawalString} \
    --withdrawal-tx-in-reference="${script_ref_utxo}#1" \
    --withdrawal-plutus-script-v2 \
    --withdrawal-reference-tx-in-redeemer-file ../data/staking/withdraw-redeemer.json \
    --tx-out="${reward_address}+${rewardBalance}" \
    --required-signer-hash ${collat_pkh} \
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
    --signing-key-file ../wallets/delegator-wallet/payment.skey \
    --signing-key-file ../wallets/collat-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx.signed