cabal clean
cabal update
cabal build -w ghc-8.10.4 -O2
cabal run nft-locking-contract
cardano-cli transaction policyid --script-file nft_locking_contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"