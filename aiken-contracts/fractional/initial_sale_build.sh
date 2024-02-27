#!/bin/bash
set -e

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

# remove and filter all traces
# aiken build

# keep the traces for testing if required
aiken build --trace-level compact --filter-traces all

# reference hash
ref=$(jq -r '.refDataHash' start_info.json)

# the starter token
pid=$(jq -r '.starterPid' start_info.json)
tkn=$(jq -r '.starterTkn' start_info.json)

# cbor representation
ref_cbor=$(python ./convert_to_cbor.py ${ref})
pid_cbor=$(python ./convert_to_cbor.py ${pid})
tkn_cbor=$(python ./convert_to_cbor.py ${tkn})

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

echo -e "\033[1;33m Convert Pointer Contract \033[0m"
aiken blueprint apply -o plutus.json -v pointer.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v pointer.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v pointer.params "${ref_cbor}"
aiken blueprint convert -v pointer.params > contracts/pointer_contract.plutus
cardano-cli transaction policyid --script-file contracts/pointer_contract.plutus > hashes/pointer_policy.hash

echo -e "\033[1;33m Convert Band Lock Contract \033[0m"
aiken blueprint apply -o plutus.json -v band_lock.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v band_lock.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v band_lock.params "${ref_cbor}"
aiken blueprint convert -v band_lock.params > contracts/band_lock_contract.plutus
cardano-cli transaction policyid --script-file contracts/band_lock_contract.plutus > hashes/band_lock.hash

echo -e "\033[1;33m Convert Batcher Token Contract \033[0m"
aiken blueprint apply -o plutus.json -v batcher.params "${pid_cbor}"
aiken blueprint apply -o plutus.json -v batcher.params "${tkn_cbor}"
aiken blueprint apply -o plutus.json -v batcher.params "${ref_cbor}"
aiken blueprint convert -v batcher.params > contracts/batcher_contract.plutus
cardano-cli transaction policyid --script-file contracts/batcher_contract.plutus > hashes/batcher.hash

###############################################################################
############## DATA NEED FOR UPDATED DATUM ####################################
###############################################################################
# validator hashes
saleHash=$(cat hashes/sale.hash)
echo Sale: $saleHash
queueHash=$(cat hashes/queue.hash)
echo Queue: $queueHash
bandHash=$(cat hashes/band_lock.hash)
echo Band: $bandHash

# pointer hash
pointerHash=$(cat hashes/pointer_policy.hash)
echo Pointer: $pointerHash
batcherHash=$(cat hashes/batcher.hash)
echo Batcher: $batcherHash

# end of build
echo -e "\033[1;32m Building Complete! \033[0m"