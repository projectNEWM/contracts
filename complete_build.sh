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

# remove old files
rm contracts/* || true
rm hashes/* || true
rm certs/* || true
rm -fr build/ || true

# build out the entire script
echo -e "\033[1;34m Building Contracts \033[0m"

# remove all traces
# aiken build --trace-level silent --filter-traces user-defined

# keep the traces
aiken build --trace-level verbose --filter-traces all

# random string
ranD=$(jq -r '.randomData' config.json)
ranS=$(jq -r '.randomStake' config.json)
ranD_cbor=$(python3 -c "import cbor2;hex_string='${ranD}';data = bytes.fromhex(hex_string);encoded = cbor2.dumps(data);print(encoded.hex())")
ranS_cbor=$(python3 -c "import cbor2;hex_string='${ranS}';data = bytes.fromhex(hex_string);encoded = cbor2.dumps(data);print(encoded.hex())")

echo -e "\033[1;33m Convert Reference Contract \033[0m"
aiken blueprint apply -o plutus.json -v reference.params "${ranD_cbor}"
aiken blueprint convert -v reference.params > contracts/reference_contract.plutus
cardano-cli transaction policyid --script-file contracts/reference_contract.plutus > hashes/reference_contract.hash

# reference hash
ref=$(cat hashes/reference_contract.hash)

# the reference token
pid=$(jq -r '.starterPid' config.json)
tkn=$(jq -r '.starterTkn' config.json)

# cbor representation
ref_cbor=$(python3 -c "import cbor2;hex_string='${ref}';data = bytes.fromhex(hex_string);encoded = cbor2.dumps(data);print(encoded.hex())")
pid_cbor=$(python3 -c "import cbor2;hex_string='${pid}';data = bytes.fromhex(hex_string);encoded = cbor2.dumps(data);print(encoded.hex())")
tkn_cbor=$(python3 -c "import cbor2;hex_string='${tkn}';data = bytes.fromhex(hex_string);encoded = cbor2.dumps(data);print(encoded.hex())")

# The pool to stake at
poolId=$(jq -r '.poolId' config.json)

echo -e "\033[1;33m Convert CIP68 Contract \033[0m"
aiken blueprint apply -o plutus.json -v storage.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v storage.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v storage.params "${ref_cbor}"
aiken blueprint convert -v storage.params > contracts/storage_contract.plutus
cardano-cli transaction policyid --script-file contracts/storage_contract.plutus > hashes/storage.hash

# build the stake contract
echo -e "\033[1;33m Convert Stake Contract \033[0m"
aiken blueprint apply -o plutus.json -v staking.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v staking.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v staking.params "${ref_cbor}"
aiken blueprint apply -o plutus.json -v staking.params "${ranS_cbor}"
aiken blueprint convert -v staking.params > contracts/stake_contract.plutus
cardano-cli transaction policyid --script-file contracts/stake_contract.plutus > hashes/stake.hash
cardano-cli stake-address registration-certificate --stake-script-file contracts/stake_contract.plutus --out-file certs/stake.cert
cardano-cli stake-address deregistration-certificate --stake-script-file contracts/stake_contract.plutus --out-file certs/de-stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file contracts/stake_contract.plutus --stake-pool-id ${poolId} --out-file certs/deleg.cert

echo -e "\033[1;33m Convert Sale Contract \033[0m"
aiken blueprint apply -o plutus.json -v sale.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v sale.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v sale.params "${ref_cbor}"
aiken blueprint convert -v sale.params > contracts/sale_contract.plutus
cardano-cli transaction policyid --script-file contracts/sale_contract.plutus > hashes/sale.hash

echo -e "\033[1;33m Convert Queue Contract \033[0m"
aiken blueprint apply -o plutus.json -v queue.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v queue.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v queue.params "${ref_cbor}"
aiken blueprint convert -v queue.params > contracts/queue_contract.plutus
cardano-cli transaction policyid --script-file contracts/queue_contract.plutus > hashes/queue.hash

echo -e "\033[1;33m Convert Minting Contract \033[0m"
aiken blueprint apply -o plutus.json -v minter.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v minter.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v minter.params "${ref_cbor}"
aiken blueprint convert -v minter.params > contracts/mint_contract.plutus
cardano-cli transaction policyid --script-file contracts/mint_contract.plutus > hashes/policy.hash

echo -e "\033[1;33m Convert Pointer Contract \033[0m"
aiken blueprint apply -o plutus.json -v pointer.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v pointer.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v pointer.params "${ref_cbor}"
aiken blueprint convert -v pointer.params > contracts/pointer_contract.plutus
cardano-cli transaction policyid --script-file contracts/pointer_contract.plutus > hashes/pointer_policy.hash

echo -e "\033[1;33m Convert Band Lock Contract \033[0m"
aiken blueprint apply -o plutus.json -v band.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v band.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v band.params "${ref_cbor}"
aiken blueprint convert -v band.params > contracts/band_lock_contract.plutus
cardano-cli transaction policyid --script-file contracts/band_lock_contract.plutus > hashes/band_lock.hash

echo -e "\033[1;33m Convert Batcher Token Contract \033[0m"
aiken blueprint apply -o plutus.json -v batcher.params "${ref_cbor}"
aiken blueprint convert -v batcher.params > contracts/batcher_contract.plutus
cardano-cli transaction policyid --script-file contracts/batcher_contract.plutus > hashes/batcher.hash

###############################################################################
############## DATUM AND REDEEMER STUFF #######################################
###############################################################################
echo -e "\033[1;33m Updating Reference Datum \033[0m"
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
storageHash=$(cat hashes/storage.hash)
saleHash=$(cat hashes/sale.hash)
queueHash=$(cat hashes/queue.hash)
bandHash=$(cat hashes/band_lock.hash)
stakeHash=$(cat hashes/stake.hash)

# pointer hash
pointerHash=$(cat hashes/pointer_policy.hash)
batcherHash=$(cat hashes/batcher.hash)

# the purchase upper bound
pqb=$(jq -r '.purchaseQueueBound' config.json)
# the refund upper bound
rqb=$(jq -r '.refundQueueBound' config.json)
# the start upper bound
ssb=$(jq -r '.startSaleBound' config.json)

# This needs to be generated from the hot key in start info.
# Assume the hot key is all the keys for now
hotKey=$(jq -r '.hotKey' config.json)

cp ./scripts/data/reference/reference-datum.json ./scripts/data/reference/backup-reference-datum.json

# update reference data
jq \
--arg hotKey "$hotKey" \
--argjson pkhs "$pkhs" \
--argjson thres "$thres" \
--arg poolId "$poolId" \
--arg rewardPkh "$rewardPkh" \
--arg rewardSc "$rewardSc" \
--arg storageHash "$storageHash" \
--arg bandHash "$bandHash" \
--arg saleHash "$saleHash" \
--arg queueHash "$queueHash" \
--arg stakeHash "$stakeHash" \
--argjson pqb "$pqb" \
--argjson rqb "$rqb" \
--argjson ssb "$ssb" \
--arg pointerHash "$pointerHash" \
--arg batcherHash "$batcherHash" \
'.fields[0].bytes=$hotKey | 
.fields[1].fields[0].list |= ($pkhs | .[0:length]) | 
.fields[1].fields[1].int=$thres | 
.fields[2].fields[0].bytes=$poolId |
.fields[2].fields[1].fields[0].bytes=$rewardPkh |
.fields[2].fields[1].fields[1].bytes=$rewardSc |
.fields[3].fields[0].bytes=$storageHash |
.fields[3].fields[1].bytes=$saleHash |
.fields[3].fields[2].bytes=$queueHash |
.fields[3].fields[3].bytes=$bandHash |
.fields[3].fields[4].bytes=$stakeHash |
.fields[4].fields[0].int=$pqb |
.fields[4].fields[1].int=$rqb |
.fields[4].fields[2].int=$ssb |
.fields[5].bytes=$pointerHash |
.fields[6].fields[2].bytes=$batcherHash
' \
./scripts/data/reference/reference-datum.json | sponge ./scripts/data/reference/reference-datum.json

# Update Staking Redeemer
echo -e "\033[1;33m Updating Stake Redeemer \033[0m"
stakeHash=$(cat_file_or_empty ./hashes/stake.hash)
jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
./scripts/data/staking/delegate-redeemer.json | sponge ./scripts/data/staking/delegate-redeemer.json

backup="./scripts/data/reference/backup-reference-datum.json"
frontup="./scripts/data/reference/reference-datum.json"

# Get the SHA-256 hash values of the files using sha256sum and command substitution
hash1=$(sha256sum "$backup" | awk '{ print $1 }')
hash2=$(sha256sum "$frontup" | awk '{ print $1 }')

# Check if the hash values are equal using string comparison in an if statement
if [ "$hash1" = "$hash2" ]; then
  echo -e "\033[1;46mNo Datum Changes Required.\033[0m"
else
  echo -e "\033[1;43mA Datum Update Is Required.\033[0m"
fi

# end of build
echo -e "\033[1;32m Building Complete! \033[0m"