# Best Practices Notes

6. A) isNInputs will stay the same for now.

7. Code Refactor will occur after best practice 8.

8. Min/Max of script will occur after audit completion with a refactor.

10. I Combined the function and changed the name to something that reflects the functionality.

12. All cabal files are the newest I can get since IOG is not putting in effort to maintain the plutus starter repo. This is the most up-to-date cabal project file I can find that actually compiles Plutus V2 smart contracts.

### Reasoning

A lot of the best practices suggested will not change validation logic nor add additional code to the contracts. Many of the best practices are already things I do in many of my other contracts and I been waiting for the audit to make these changes anyway. The audit fixes will start with the implementation of best practices. The end goal after the audit is a nice code refactoring and use of every plutonomy suggestion to min/max the contract.

# QSP-1 Lack of Comprehensive Test Suite

- Will Parameterize the contract and write tests

# QSP-2 Risk of Starter Token Duplicates and Tokenized Token Name Clashing

- Write a response to this.

# QSP-3 Dangerous Use of PlutusTx.unstableMakeIsData

If Plutus were to change the ```defaultIndex``` then the indices of a data constructor may not be stable but by using ```makeIsDataIndexed``` it will force the data constructor to a specific index.

```hs
defaultIndex :: TH.Name -> TH.Q [(TH.Name, Int)]
defaultIndex name = do
    info <- TH.reifyDatatype name
    pure $ zip (TH.constructorName <$> TH.datatypeCons info) [0..]

-- | Generate a 'FromData' and a 'ToData' instance for a type. This may not be stable in the face of constructor additions,
-- renamings, etc. Use 'makeIsDataIndexed' if you need stability.
unstableMakeIsData :: TH.Name -> TH.Q [TH.Dec]
unstableMakeIsData name = makeIsDataIndexed name =<< defaultIndex name
```

All occurences of 

```hs
PlutusTx.unstableMakeIsData ''CustomDatumType
```

are now switched into

```hs
PlutusTx.makeIsDataIndexed ''CustomDatumType [('CustomDatumType, 0)]
```

# QSP-4 Privileged Roles and Owners

- Write a response to this.

# QSP-5 No Update Mechanism in Case of Rogue Multisig Users

- Write a response to this.

# QSP-6 Only the Artist Can Receive Solidified NFT Even Though All the Fractions Might Be Owned by Someone Else

The limitation that only the artist inside the datum may receive the tokenized NFT after solidifying the fractional tokens is incorrect. The validation logic should mirror the other endpoints by permitting NEWM to decide the destination address. The validation logic now allows the tokenized NFT to be sent to the wallet that held the fractional tokens. Now all the endpoints should have the same destination validation.

This changed the datum and the validation logic of unlocking a tokenized NFT.

# QSP-7 Malicious Users Could Sabotage Solidification of Fractionalized Nfts by Holding Off Fractionalized Tokens

- This was intentional with fractionalization. Write a response to this.

# QSP-8 Dangling UTXO at Locking Contract Validation Script

- The off chain needs to account for this potential edge case. Work around is minting fractions then burning the fractions to remove dangling UTxO.

# QSP-9 Artist Receives Min-UTXO-Deposit in Addition to NFT when Solidifying NFT

- This has been removed and should not be an issue due to the qsp-6 fix. The receiver of the Tokenized NFT after solidifying the fractions will get the minimum required ADA for that UTxO and not the validating value.

# QSP-10 Redundant Check of Validator’s Output-Datum in NFTMintingContract and MintingContract

Upon further inspection there are many redudancies between the lock and the mint contracts. Notably, signing and value checking. The datum comparision is the obvious redundancy. I will remove the redundancy from  the tokenization for now but will want to come back to this to see if more can be removed safely.

# QSP-11 Lack of Documentation

- Improved documentation will be written.

# QSP-12 Importance of Secure Key Management in the Project

- Write a response to this.
