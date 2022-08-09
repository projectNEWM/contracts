cabal build -w ghc-8.10.7
cabal run v2-nft-minting-contract
cardano-cli transaction policyid --script-file v2-tokenized-minting-contract.plutus > policy.id
echo "POLICY ID:" $(cat policy.id)
echo "DONE"