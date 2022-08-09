cabal build -w ghc-8.10.7
cabal run v2-nft-locking-contract
cardano-cli transaction policyid --script-file v2-tokenized-locking-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"