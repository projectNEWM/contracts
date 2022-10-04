cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run marketplace-contract
cardano-cli transaction policyid --script-file marketplace-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"