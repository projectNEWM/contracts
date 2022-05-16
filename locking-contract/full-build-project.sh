cabal clean
cabal update
cabal build -w ghc-8.10.4 -O2
cabal run locking-contract
cardano-cli transaction policyid --script-file locking_contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"