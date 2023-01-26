rm validator.addr
rm validator.hash
rm validator.bytes
rm fractional-sale-contract.plutus

# build the script
cabal build -w ghc-8.10.7
cabal run fractional-sale-contract

#
cardano-cli address build --payment-script-file fractional-sale-contract.plutus --testnet-magic 2 --out-file validator.addr
cardano-cli transaction policyid --script-file fractional-sale-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

#
echo -e "\033[1;36m\nValidator Addr: $(cat validator.addr) \033[0m"
echo -e "\033[1;36mValidator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36mValidator Byte: $(cat validator.bytes) \033[0m"
echo
echo "DONE"