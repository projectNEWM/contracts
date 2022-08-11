# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# set up start info
policy_id=$(cat tokenize-scripts/policy/policy.id)
# It'sTheStarterToken4ProjectNewM
token_name="4974277354686553746172746572546f6b656e3450726f6a6563744e65774d"
variable=${policy_id}; jq --arg variable "$variable" '.starterPid=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json
variable=${token_name}; jq --arg variable "$variable" '.starterTkn=$variable' start_info.json > start_info-new.json
mv start_info-new.json start_info.json

# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn
python3 -c "import binascii;a=$(cat start_info.json | jq .delegator);s=binascii.unhexlify(a);print([x for x in s])" > deleg.pkh

# Add in the 2 out of 3 multisig
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig1);s=binascii.unhexlify(a);print([x for x in s])" > multisig1.pkh
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig2);s=binascii.unhexlify(a);print([x for x in s])" > multisig2.pkh
python3 -c "import binascii;a=$(cat start_info.json | jq .multisig3);s=binascii.unhexlify(a);print([x for x in s])" > multisig3.pkh

python3 -c "from update_contracts import changeMultiPkh;changeMultiPkh('./nft-locking-contract/src/NFTLockingContract.hs', './nft-locking-contract/src/NFTLockingContract.hs-new.hs', $(cat multisig1.pkh), $(cat multisig2.pkh), $(cat multisig3.pkh))"
mv ./nft-locking-contract/src/NFTLockingContract.hs-new.hs ./nft-locking-contract/src/NFTLockingContract.hs
python3 -c "from update_contracts import changeMultiPkh;changeMultiPkh('./nft-minting-contract/src/NFTMintingContract.hs', './nft-minting-contract/src/NFTMintingContract.hs-new.hs', $(cat multisig1.pkh), $(cat multisig2.pkh), $(cat multisig3.pkh))"
mv ./nft-minting-contract/src/NFTMintingContract.hs-new.hs ./nft-minting-contract/src/NFTMintingContract.hs

# Adds the delegator to the nft locking and minting contracts
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./nft-locking-contract/src/NFTLockingContract.hs', './nft-locking-contract/src/NFTLockingContract.hs-new.hs', $(cat deleg.pkh))"
mv ./nft-locking-contract/src/NFTLockingContract.hs-new.hs ./nft-locking-contract/src/NFTLockingContract.hs
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./nft-minting-contract/src/NFTMintingContract.hs', './nft-minting-contract/src/NFTMintingContract.hs-new.hs', $(cat deleg.pkh))"
mv ./nft-minting-contract/src/NFTMintingContract.hs-new.hs ./nft-minting-contract/src/NFTMintingContract.hs

# Adds the delegator to the ft locking and minting contracts
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract.hs-new.hs', $(cat deleg.pkh))"
mv ./locking-contract/src/LockingContract.hs-new.hs ./locking-contract/src/LockingContract.hs
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract.hs-new.hs', $(cat deleg.pkh))"
mv ./minting-contract/src/MintingContract.hs-new.hs ./minting-contract/src/MintingContract.hs


# Adds in the locking token into the contract.
python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./nft-locking-contract/src/NFTLockingContract.hs', './nft-locking-contract/src/NFTLockingContract-new.hs', $(cat start.pid))"
mv ./nft-locking-contract/src/NFTLockingContract-new.hs ./nft-locking-contract/src/NFTLockingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./nft-locking-contract/src/NFTLockingContract.hs', './nft-locking-contract/src/NFTLockingContract-new.hs', $(cat start.tkn))"
mv ./nft-locking-contract/src/NFTLockingContract-new.hs ./nft-locking-contract/src/NFTLockingContract.hs

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./nft-minting-contract/src/NFTMintingContract.hs', './nft-minting-contract/src/NFTMintingContract-new.hs', $(cat start.pid))"
mv ./nft-minting-contract/src/NFTMintingContract-new.hs ./nft-minting-contract/src/NFTMintingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./nft-minting-contract/src/NFTMintingContract.hs', './nft-minting-contract/src/NFTMintingContract-new.hs', $(cat start.tkn))"
mv ./nft-minting-contract/src/NFTMintingContract-new.hs ./nft-minting-contract/src/NFTMintingContract.hs


# build
cd nft-locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run nft-locking-contract
cardano-cli transaction policyid --script-file nft-locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# nft locking validator hash
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"


cd ..


# adds in the locking hash into the script
python3 -c "from update_contracts import changeLockHash;changeLockHash('./nft-minting-contract/src/NFTMintingContract.hs', './nft-minting-contract/src/NFTMintingContract-new.hs', $(cat ./nft-locking-contract/validator.bytes))"
mv ./nft-minting-contract/src/NFTMintingContract-new.hs ./nft-minting-contract/src/NFTMintingContract.hs


# build nft minting
cd nft-minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run nft-minting-contract
cardano-cli transaction policyid --script-file nft-minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

# nft minting validator hash
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
python3 -c "from update_contracts import changeTokenizedPid;changeTokenizedPid('./locking-contract/src/LockingContract.hs', './locking-contract/src/LockingContract-new.hs', $(cat ./nft-minting-contract/policy.bytes) )"
mv ./locking-contract/src/LockingContract-new.hs ./locking-contract/src/LockingContract.hs

cd locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run locking-contract
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

# adds in the locking hash into the script
cd ..
python3 -c "from update_contracts import changeLockHash;changeLockHash('./minting-contract/src/MintingContract.hs', './minting-contract/src/MintingContract-new.hs', $(cat ./locking-contract/validator.bytes))"
mv ./minting-contract/src/MintingContract-new.hs ./minting-contract/src/MintingContract.hs

cd minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7
cabal run minting-contract
cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"

cd ../fractionalize-scripts/data
variable=$(cat ../../minting-contract/policy.id); jq --arg variable "$variable" '.fields[0].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
variable=$(cat ../../nft-minting-contract/policy.id); jq --arg variable "$variable" '.fields[1].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
# build fractionalize contractsd