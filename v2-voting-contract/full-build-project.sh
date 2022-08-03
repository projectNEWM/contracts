echo "Voting script compile"
rm validator.bytes
rm validator.hash
cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run v2-voting-contract
cardano-cli transaction policyid --script-file v2-voting-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo "Validator Bytes:" $(cat validator.bytes)