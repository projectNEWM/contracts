cabal clean
cabal update
cabal build -w ghc-8.10.4 -O2
cabal run nft-minting-contract
cardano-cli transaction policyid --script-file nft_minting_contract.plutus > policy.id
echo "DONE"