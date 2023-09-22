rm validator.hash
cabal build -w ghc-8.10.7
cabal run nft-locking-contract
cardano-cli transaction policyid --script-file nft-locking-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"