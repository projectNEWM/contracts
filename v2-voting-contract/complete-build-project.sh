echo -e "\033[1;35m Update all contracts with Vote start NFT. \033[0m" 
# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .startVotePid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .startVoteTkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn
python3 -c "import binascii;a=$(cat start_info.json | jq .delegator);s=binascii.unhexlify(a);print([x for x in s])" > deleg.pkh

# Adds the delegator to the DID minting contract
python3 -c "from update_contracts import changeDelegPkh;changeDelegPkh('../v2-did-minting-contract/src/V2DidMintingContract.hs', '../v2-did-minting-contract/src/V2DidMintingContract-new.hs', $(cat deleg.pkh))"
mv ../v2-did-minting-contract/src/V2DidMintingContract-new.hs ../v2-did-minting-contract/src/V2DidMintingContract.hs

# Adds the starter nft for voting into contracts.
python3 -c "from update_contracts import changeStartPid;changeStartPid('../v2-voting-contract/src/V2VotingContract.hs', '../v2-voting-contract/src/V2VotingContract-new.hs', $(cat start.pid))"
mv ../v2-voting-contract/src/V2VotingContract-new.hs ../v2-voting-contract/src/V2VotingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('../v2-voting-contract/src/V2VotingContract.hs', '../v2-voting-contract/src/V2VotingContract-new.hs', $(cat start.tkn))"
mv ../v2-voting-contract/src/V2VotingContract-new.hs ../v2-voting-contract/src/V2VotingContract.hs
# did locking
python3 -c "from update_contracts import changeStartPid;changeStartPid('../v2-did-locking-contract/src/V2DidLockingContract.hs', '../v2-did-locking-contract/src/V2DidLockingContract-new.hs', $(cat start.pid))"
mv ../v2-did-locking-contract/src/V2DidLockingContract-new.hs ../v2-did-locking-contract/src/V2DidLockingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('../v2-did-locking-contract/src/V2DidLockingContract.hs', '../v2-did-locking-contract/src/V2DidLockingContract-new.hs', $(cat start.tkn))"
mv ../v2-did-locking-contract/src/V2DidLockingContract-new.hs ../v2-did-locking-contract/src/V2DidLockingContract.hs
# nft locking
python3 -c "from update_contracts import changeStartPid;changeStartPid('../v2-nft-locking-contract/src/V2NFTLockingContract.hs', '../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs', $(cat start.pid))"
mv ../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs ../v2-nft-locking-contract/src/V2NFTLockingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('../v2-nft-locking-contract/src/V2NFTLockingContract.hs', '../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs', $(cat start.tkn))"
mv ../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs ../v2-nft-locking-contract/src/V2NFTLockingContract.hs
# ft locking
python3 -c "from update_contracts import changeStartPid;changeStartPid('../v2-locking-contract/src/V2LockingContract.hs', '../v2-locking-contract/src/V2LockingContract-new.hs', $(cat start.pid))"
mv ../v2-locking-contract/src/V2LockingContract-new.hs ../v2-locking-contract/src/V2LockingContract.hs
python3 -c "from update_contracts import changeStartTkn;changeStartTkn('../v2-locking-contract/src/V2LockingContract.hs', '../v2-locking-contract/src/V2LockingContract-new.hs', $(cat start.tkn))"
mv ../v2-locking-contract/src/V2LockingContract-new.hs ../v2-locking-contract/src/V2LockingContract.hs
#
# exit
#
echo -e "\033[1;33m Compile Voting Script \033[0m"
# Build Voting contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run v2-voting-contract
cardano-cli transaction policyid --script-file v2-voting-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# voting validator hash
echo -e "\033[1;36m Voting Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m voting Validator Bytes: $(cat validator.bytes) \033[0m"
#
echo -e "\033[1;35m Updating Locking contracts With Voting Data \033[0m"
# did lock
python3 -c "from update_contracts import changeVoteHash;changeVoteHash('../v2-did-locking-contract/src/V2DidLockingContract.hs', '../v2-did-locking-contract/src/V2DidLockingContract-new.hs', $(cat validator.bytes))"
mv ../v2-did-locking-contract/src/V2DidLockingContract-new.hs ../v2-did-locking-contract/src/V2DidLockingContract.hs
# nft lock
python3 -c "from update_contracts import changeVoteHash;changeVoteHash('../v2-nft-locking-contract/src/V2NFTLockingContract.hs', '../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs', $(cat validator.bytes))"
mv ../v2-nft-locking-contract/src/V2NFTLockingContract-new.hs ../v2-nft-locking-contract/src/V2NFTLockingContract.hs
# ft lock
python3 -c "from update_contracts import changeVoteHash;changeVoteHash('../v2-locking-contract/src/V2LockingContract.hs', '../v2-locking-contract/src/V2LockingContract-new.hs', $(cat validator.bytes))"
mv ../v2-locking-contract/src/V2LockingContract-new.hs ../v2-locking-contract/src/V2LockingContract.hs
#
# exit
#
echo -e "\033[1;33m Compile DID Locking Script \033[0m"
cd ../v2-did-locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run v2-did-locking-contract
cardano-cli transaction policyid --script-file v2-did-locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
# did locking validator hash
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

echo -e "\033[1;35m Updating DID Minting contract With DID Locking Data \033[0m"
cd ../v2-voting-contract
python3 -c "from update_contracts import changeLockHash;changeLockHash('../v2-did-minting-contract/src/V2DidMintingContract.hs', '../v2-did-minting-contract/src/V2DidMintingContract-new.hs', $(cat ../v2-did-locking-contract/validator.bytes))"
mv ../v2-did-minting-contract/src/V2DidMintingContract-new.hs ../v2-did-minting-contract/src/V2DidMintingContract.hs
#
#exit
#
echo -e "\033[1;33m Compile DID Minting Script \033[0m"
cd ../v2-did-minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run v2-did-minting-contract
cardano-cli transaction policyid --script-file v2-did-minting-contract.plutus > policy.id
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes
# did minting validator hash
echo -e "\033[1;36m Policy Id: $(cat policy.id) \033[0m"
echo -e "\033[1;36m Policy Bytes: $(cat policy.bytes) \033[0m"

# update datums for did contract
cd ../did-scripts/data
variable=$(cat ../../v2-voting-contract/start_info.json | jq -r .delegator); jq --arg variable $variable '.fields[0].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
variable=$(cat ../../v2-did-minting-contract/policy.id); jq --arg variable "$variable" '.fields[1].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
variable=$(cat ../../v2-did-locking-contract/validator.hash); jq --arg variable "$variable" '.fields[2].bytes=$variable' datum.json > datum-new.json
mv datum-new.json datum.json
echo -e "\033[1;36m DID Datum \033[0m"
cat datum.json | jq
#
echo "DONE"