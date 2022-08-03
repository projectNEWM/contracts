echo "Voting script compile"
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run v2-voting-contract
cardano-cli transaction policyid --script-file v2-voting-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo "Validator Bytes:" $(cat validator.bytes)

echo "update did locking contract from voting contract"
python3 -c "from update_contracts import changeVoteHash;changeVoteHash('../v2-did-locking-contract/src/V2DidLockingContract.hs', '../v2-did-locking-contract/src/V2DidLockingContract-new.hs', $(cat validator.bytes))"
mv ../v2-did-locking-contract/src/V2DidLockingContract-new.hs ../v2-did-locking-contract/src/V2DidLockingContract.hs

cd ../v2-did-locking-contract
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7 -O2
cabal run v2-did-locking-contract
cardano-cli transaction policyid --script-file v2-did-locking-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo "Validator Bytes:" $(cat validator.bytes)

echo "updating minting contract from locking contract"
cd ../v2-voting-contract
python3 -c "from update_contracts import changeLockHash;changeLockHash('../v2-did-minting-contract/src/V2DidMintingContract.hs', '../v2-did-minting-contract/src/V2DidMintingContract-new.hs', $(cat validator.bytes))"
mv ../v2-did-minting-contract/src/V2DidMintingContract-new.hs ../v2-did-minting-contract/src/V2DidMintingContract.hs

cd ../v2-did-minting-contract
rm policy.id
rm policy.bytes
cabal build -w ghc-8.10.7 -O2
cabal run v2-did-minting-contract

cardano-cli transaction policyid --script-file v2-did-minting-contract.plutus > policy.id

echo "Policy Id:" $(cat policy.id)

python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes

echo "Policy Bytes:" $(cat policy.bytes)

echo "DONE"