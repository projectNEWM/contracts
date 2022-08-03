rm policy.bytes
rm policy.hash
cabal build -w ghc-8.10.7
cabal run v2-did-minting-contract
cardano-cli transaction policyid --script-file v2-did-minting-contract.plutus > policy.id
echo "Policy Id:" $(cat policy.id)
python3 -c "import binascii;a='$(cat policy.id)';s=binascii.unhexlify(a);print([x for x in s])" > policy.bytes
echo "Policy Bytes:" $(cat policy.bytes)
echo "DONE"