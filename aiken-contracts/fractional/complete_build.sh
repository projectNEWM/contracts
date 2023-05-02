#!/bin/bash
set -e

function cat_file_or_empty() {
  if [ -e "$1" ]; then
    cat "$1"
  else
    echo ""
  fi
}

# create directories if dont exist
mkdir -p contracts
mkdir -p hashes
mkdir -p certs

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"
aiken build

echo -e "\033[1;33m Convert Reference Contract \033[0m"
aiken blueprint convert -v data_reference.data_reference > contracts/reference_contract.plutus
cardano-cli transaction policyid --script-file contracts/reference_contract.plutus > hashes/reference_contract.hash

# reference hash
ref=$(cat hashes/reference_contract.hash)

# the reference token
pid=$(jq -r '.starterPid' start_info.json)
tkn=$(jq -r '.starterTkn' start_info.json)


# cbor representation
ref_cbor=$(python ./convert_to_cbor.py ${ref})
pid_cbor=$(python ./convert_to_cbor.py ${pid})
tkn_cbor=$(python ./convert_to_cbor.py ${tkn})

# The pool to stake at
poolId=$(jq -r '.poolId' start_info.json)

echo -e "\033[1;33m Convert CIP68 Contract \033[0m"
aiken blueprint apply -o plutus.json -v cip68.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v cip68.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v cip68.params "${ref_cbor}" .
aiken blueprint convert -v cip68.params > contracts/cip68_contract.plutus
cardano-cli transaction policyid --script-file contracts/cip68_contract.plutus > hashes/cip68.hash


# build the stake contract

ran=$(jq -r '.random' start_info.json)
ran_cbor=$(python ./convert_to_cbor.py ${ran})


echo -e "\033[1;33m Convert Stake Contract \033[0m"
aiken blueprint apply -o plutus.json -v staking.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v staking.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v staking.params "${ref_cbor}" .
aiken blueprint apply -o plutus.json -v staking.params "${ran_cbor}" .
aiken blueprint convert -v staking.params > contracts/stake_contract.plutus
cardano-cli transaction policyid --script-file contracts/stake_contract.plutus > hashes/stake.hash
cardano-cli stake-address registration-certificate --stake-script-file contracts/stake_contract.plutus --out-file certs/stake.cert
cardano-cli stake-address deregistration-certificate --stake-script-file contracts/stake_contract.plutus --out-file certs/de-stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file contracts/stake_contract.plutus --stake-pool-id ${poolId} --out-file certs/deleg.cert

echo -e "\033[1;33m Convert Sale Contract \033[0m"
aiken blueprint convert -v sale.sale > contracts/sale_contract.plutus
cardano-cli transaction policyid --script-file contracts/sale_contract.plutus > hashes/sale.hash


echo -e "\033[1;33m Convert Minting Contract \033[0m"
aiken blueprint apply -o plutus.json -v minter.params "${pid_cbor}" .
aiken blueprint apply -o plutus.json -v minter.params "${tkn_cbor}" .
aiken blueprint apply -o plutus.json -v minter.params "${ref_cbor}" .
aiken blueprint convert -v minter.params > contracts/mint_contract.plutus
cardano-cli transaction policyid --script-file contracts/mint_contract.plutus > hashes/policy.hash

###############DATUM AND REDEEMER STUFF
echo -e "\033[1;33m Updating Reference Datum \033[0m"
# # build out the reference datum data
caPkh=$(cat_file_or_empty ./scripts/wallets/newm-wallet/payment.hash)
# keepers
pkh1=$(cat_file_or_empty ./scripts/wallets/keeper1-wallet/payment.hash)
pkh2=$(cat_file_or_empty ./scripts/wallets/keeper2-wallet/payment.hash)
pkh3=$(cat_file_or_empty ./scripts/wallets/keeper3-wallet/payment.hash)
pkhs="[{\"bytes\": \"$pkh1\"}, {\"bytes\": \"$pkh2\"}, {\"bytes\": \"$pkh3\"}]"
thres=2
# pool stuff
rewardPkh=$(cat_file_or_empty ./scripts/wallets/reward-wallet/payment.hash)
rewardSc=""
# validator hashes
cip68Hash=$(cat hashes/cip68.hash)
saleHash=$(cat hashes/sale.hash)
stakeHash=$(cat hashes/stake.hash)

# update reference data
jq \
--arg caPkh "$caPkh" \
--argjson pkhs "$pkhs" \
--argjson thres "$thres" \
--arg poolId "$poolId" \
--arg rewardPkh "$rewardPkh" \
--arg rewardSc "$rewardSc" \
--arg cip68Hash "$cip68Hash" \
--arg saleHash "$saleHash" \
--arg stakeHash "$stakeHash" \
'.fields[0].bytes=$caPkh | 
.fields[1].fields[0].list |= ($pkhs | .[0:length]) | 
.fields[1].fields[1].int=$thres | 
.fields[2].fields[0].bytes=$poolId |
.fields[2].fields[1].bytes=$rewardPkh |
.fields[2].fields[2].bytes=$rewardSc |
.fields[3].fields[0].bytes=$cip68Hash |
.fields[3].fields[1].bytes=$saleHash |
.fields[3].fields[2].bytes=$stakeHash
' \
./scripts/data/reference/reference-datum.json | sponge ./scripts/data/reference/reference-datum.json

# Update Staking Redeemer
echo -e "\033[1;33m Updating Stake Redeemer \033[0m"
stakeHash=$(cat_file_or_empty ./hashes/stake.hash)
jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
./scripts/data/staking/delegate-redeemer.json | sponge ./scripts/data/staking/delegate-redeemer.json

echo -e "\033[1;32m Building Complete! \033[0m"
