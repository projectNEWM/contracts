rm policy.id
cabal build -w ghc-8.10.7
cabal run minting-contract
cardano-cli transaction policyid --script-file minting-contract.plutus > policy.id
echo "POLICY ID:" $(cat policy.id)
echo "DONE"