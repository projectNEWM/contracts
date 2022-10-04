cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run marketplace-contract
#
cardano-cli address build --payment-script-file marketplace-contract.plutus --testnet-magic 2 --out-file validator.addr
cardano-cli transaction policyid --script-file marketplace-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo
echo "Validator Hash:" $(cat validator.hash)
echo "Validator Addr:" $(cat validator.addr)
echo "Validator Bytes:" $(cat validator.bytes)
echo "DONE"