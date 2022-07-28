cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run v2-nft-minting-contract
cardano-cli transaction policyid --script-file v2-tokenized-minting-contract.plutus > policy.id
echo "Policy ID:" $(cat policy.id)
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes
echo "DONE"