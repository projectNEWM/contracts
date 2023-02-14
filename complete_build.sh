#!/bin/bash
# set -e
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Token Name That Will Be Used For The Starter NFT And The Catalog Name'
    exit 1
fi

source ./.env

# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# Populate start_info.json with the keys we actually are using
variable=$(./tokenize-scripts/bech32 < ./tokenize-scripts/wallets/delegator-wallet/payment.addr | cut -c 3-); jq --arg variable "$variable" '.delegator=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=$(./tokenize-scripts/bech32 < ./tokenize-scripts/wallets/multisig-wallet/multisig1.addr | cut -c 3-); jq --arg variable "$variable" '.multisig1=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=$(./tokenize-scripts/bech32 < ./tokenize-scripts/wallets/multisig-wallet/multisig2.addr | cut -c 3-); jq --arg variable "$variable" '.multisig2=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=$(./tokenize-scripts/bech32 < ./tokenize-scripts/wallets/multisig-wallet/multisig3.addr | cut -c 3-); jq --arg variable "$variable" '.multisig3=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json

pkh1=$(cat start_info.json | jq -r .multisig1)
pkh2=$(cat start_info.json | jq -r .multisig2)
pkh3=$(cat start_info.json | jq -r .multisig3)

# set up start info
variable=${pkh1}; jq --arg variable "$variable" '.scripts[0].keyHash=$variable' tokenize-scripts/policy/policy.script > tokenize-scripts/policy/policy-new.script
mv tokenize-scripts/policy/policy-new.script tokenize-scripts/policy/policy.script
variable=${pkh2}; jq --arg variable "$variable" '.scripts[1].keyHash=$variable' tokenize-scripts/policy/policy.script > tokenize-scripts/policy/policy-new.script
mv tokenize-scripts/policy/policy-new.script tokenize-scripts/policy/policy.script
variable=${pkh3}; jq --arg variable "$variable" '.scripts[2].keyHash=$variable' tokenize-scripts/policy/policy.script > tokenize-scripts/policy/policy-new.script
mv tokenize-scripts/policy/policy-new.script tokenize-scripts/policy/policy.script

${cli} transaction policyid --script-file tokenize-scripts/policy/policy.script > tokenize-scripts/policy/starter.id
policy_id=$(cat tokenize-scripts/policy/starter.id)

# Create the payment addr for the script multisig wallet
${cli} address build --payment-script-file ./tokenize-scripts/policy/policy.script ${network} > ./tokenize-scripts/wallets/multisig-wallet/payment.addr

tkn_name=${1}
tkn_name=$(echo ${tkn_name:0:32})
echo -e "\033[1;36m Token Name: ${tkn_name} \033[0m"

token_name=$(echo -n ${tkn_name} | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
echo -e "\033[1;36m Starter Token ${policy_id}.${token_name} \033[0m"

variable=${policy_id}; jq --arg variable "$variable" '.starterPid=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=${token_name}; jq --arg variable "$variable" '.starterTkn=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json


variable=${token_name}; jq --arg variable "$variable" '.fields[2].bytes=$variable' tokenize-scripts/data/current_datum.json > tokenize-scripts/data/current_datum-new.json
mv tokenize-scripts/data/current_datum-new.json tokenize-scripts/data/current_datum.json
variable=${token_name}; jq --arg variable "$variable" '.fields[2].bytes=$variable' tokenize-scripts/data/next_datum.json > tokenize-scripts/data/next_datum-new.json
mv tokenize-scripts/data/next_datum-new.json tokenize-scripts/data/next_datum.json

#
# exit
#
# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn
python3 -c "import binascii;a=$(cat start_info.json | jq .delegator);s=binascii.unhexlify(a);print([x for x in s])" > deleg.pkh

# Add in the 2 out of 3 multisig
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig1);s=binascii.unhexlify(a);print([x for x in s])" > multisig1.pkh
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig2);s=binascii.unhexlify(a);print([x for x in s])" > multisig2.pkh
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig3);s=binascii.unhexlify(a);print([x for x in s])" > multisig3.pkh

# update the nft locking contract information
jq \
--argjson pid "$(cat start.pid)" \
--argjson tkn "$(cat start.tkn)" \
--argjson pkh "$(cat deleg.pkh)" \
--argjson multisig1 "$(cat multisig1.pkh)" \
--argjson multisig2 "$(cat multisig2.pkh)" \
--argjson multisig3 "$(cat multisig3.pkh)" \
'.pid = $pid | .tkn = $tkn | .pkh = $pkh | .multisig1 = $multisig1 | .multisig2 = $multisig2 | .multisig3 = $multisig3' \
nft-locking-contract/nft_locking_info.json | sponge nft-locking-contract/nft_locking_info.json


# build
cd nft-locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run nft-locking-contract
${cli} transaction policyid --script-file nft-locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# nft locking validator hash
echo -e "\033[1;36m NFT LOCKING CONTRACT \033[0m"
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

cd ..

# update the nft minting contract information
jq \
--argjson pid "$(cat start.pid)" \
--argjson tkn "$(cat start.tkn)" \
--argjson pkh "$(cat deleg.pkh)" \
--argjson multisig1 "$(cat multisig1.pkh)" \
--argjson multisig2 "$(cat multisig2.pkh)" \
--argjson multisig3 "$(cat multisig3.pkh)" \
--argjson valid "$(cat nft-locking-contract/validator.bytes)" \
'.pid = $pid | .tkn = $tkn | .pkh = $pkh | .multisig1 = $multisig1 | .multisig2 = $multisig2 | .multisig3 = $multisig3 | .valid = $valid' \
nft-minting-contract/nft_minting_info.json | sponge nft-minting-contract/nft_minting_info.json


# build nft minting
cd nft-minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run nft-minting-contract
${cli} transaction policyid --script-file nft-minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting validator hash
echo -e "\033[1;36m NFT MINTING CONTRACT \033[0m"
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"

# update the datums for the nft generator
cd ../tokenize-scripts/data
variable=$(cat ../../nft-minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' current_datum.json > current_datum-new.json
mv current_datum-new.json current_datum.json
variable=$(cat ../../nft-minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' next_datum.json > next_datum-new.json
mv next_datum-new.json next_datum.json

# update fractionalize contracts
cd ../..

# update the nft minting contract information
jq \
--argjson pid "$(cat ./nft-minting-contract/policy.bytes)" \
--argjson pkh "$(cat deleg.pkh)" \
'.pid = $pid | .pkh = $pkh' \
locking-contract/locking_info.json | sponge locking-contract/locking_info.json

cd locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run locking-contract
${cli} transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo -e "\033[1;36m LOCKING CONTRACT \033[0m"
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

# adds in the locking hash into the script
cd ..

# update the nft minting contract information
jq \
--argjson valid "$(cat ./locking-contract/validator.bytes)" \
--argjson pkh "$(cat deleg.pkh)" \
'.pkh = $pkh | .valid = $valid' \
minting-contract/minting_info.json | sponge minting-contract/minting_info.json

cd minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7
cabal run minting-contract
${cli} transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo -e "\033[1;36m MINTING CONTRACT \033[0m"
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"

cd ../fractionalize-scripts/data
variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
variable=$(cat ../../nft-minting-contract/policy.id); jq --arg variable "$variable" '.fields[1].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json

cd ../..

find . -name '*.hash' -type f -exec sha256sum {} \; > hash.hashes
echo -e "\033[1;36m \nvalidator sha256sum \033[0m"
echo -e "\033[1;33m $(cat hash.hashes) \033[0m"

find . -name 'policy.id' -type f -exec sha256sum {} \; > policy.hashes
echo -e "\033[1;36m policy sha256sum \033[0m"
echo -e "\033[1;33m $(cat policy.hashes) \033[0m"

find . -name '*.hashes' -type f -exec sha256sum {} \; > final.check
echo -e "\033[1;35m \nfinal sha256sum \033[0m"
echo -e "\033[1;32m $(sha256sum final.check) \033[0m"
