cabal build -w ghc-8.10.4
cabal run minting-contract
cardano-cli transaction policyid --script-file minting_contract.plutus > policy.id
echo "POLICY ID:" $(cat policy.id)
echo "DONE"