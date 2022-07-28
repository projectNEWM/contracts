cabal build -w ghc-8.10.7 -O2
cabal run v2-voting-contract
cardano-cli transaction policyid --script-file v2-voting-contract.plutus > validator.hash
echo "Validator Hash:" $(cat validator.hash)
echo "DONE"