# Audit Notes

## Best Practice Reasoning

A lot of the best practices suggested will not change validation logic nor add additional code to the contracts. Many of the best practices are already things I do in many of my other contracts and I been waiting for the audit to make these changes anyway. The audit fixes will start with the implementation of best practices. The end goal after the audit is a nice code refactoring and use of every plutonomy suggestion to min/max the contract.

## Some notes on best practices

6. A) isNInputs will stay the same for now as this requires a new version of useful-funcs.

7. Code Refactor will occur after best practice 8.

8. Min/Max of script will occur after audit completion. Will refactor then min-max.

10. I Combined the function and changed the name to something that reflects the functionality.

12. All cabal files are the newest I can get since IOG is not putting in effort to maintain the plutus starter repo. This is the most up-to-date cabal project file I can find that actually compiles Plutus V2 smart contracts. If you have access to an actually up-to-do cabal project file that compiles V2 it would be very helpful.


# QSP-1 Lack of Comprehensive Test Suite

All contracts are parameterized. The script parameters are known at compile time by importing data from json files in each contract folder. Each contract is tested with the new test-suite. 

This took a lot of my time. I have realized that many open source projects either do not write tests or they just use the cli to write tests. Even the contracts IOG provides have just been updated to V2 and most do not cover lock-n-mint style contracts or even contracts with many datums/redeemers. I spent more time trying to just get Plutus-Apps testing to work then the time spent on creating my own test-suite. 

Testing plutus contracts with Plutus-Apps is a horrible experience. So bad in fact I wrote my own test-suite because of it. There are instructions to the test-suite inside the `test-suite` folder. I imagine there can always be more tests written but this is the minimal tests required to hit each trace in the validation logic of an endpoint.

# QSP-2 Risk of Starter Token Duplicates and Tokenized Token Name Clashing

A response is written to this inside the docs folder.

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

A response is written to this inside the docs folder.

# QSP-5 No Update Mechanism in Case of Rogue Multisig Users

A response is written to this inside the docs folder.

# QSP-6 Only the Artist Can Receive Solidified NFT Even Though All the Fractions Might Be Owned by Someone Else

The limitation that only the artist inside the datum may receive the tokenized NFT after solidifying the fractional tokens is incorrect. The validation logic should mirror the other endpoints by permitting NEWM to decide the destination address. The validation logic now allows the tokenized NFT to be sent to the wallet that held the fractional tokens. Now all the endpoints should have the same destination validation. It is intentional that NEWM controls the destination.

This changed the datum and the validation logic of unlocking a tokenized NFT.

# QSP-7 Malicious Users Could Sabotage Solidification of Fractionalized Nfts by Holding Off Fractionalized Tokens

This was intentional with fractionalization. The act of tokenization and fractionalize implies that the fractions will be distributed.

# QSP-8 Dangling UTxO at Locking Contract Validation Script

The off chain should account for this potential edge case but the code will remain as it for now. The typical use case seen at NEWM for every artist at this point is tokenization and fractionalization. The work around for this dangling UTxO is minting fractions then burning the fractions to remove dangling UTxO. 

# QSP-9 Artist Receives Min-UTXO-Deposit in Addition to NFT when Solidifying NFT

This has been removed and should not be an issue due to the qsp-6 fix. The receiver of the Tokenized NFT after solidifying the fractions will get the minimum required ADA for that UTxO and not the validating value.

# QSP-10 Redundant Check of Validator’s Output-Datum in NFTMintingContract and MintingContract

Upon further inspection there are many redudancies between the lock and the mint contracts. Notably, signing and value checking. The datum comparision is the obvious redundancy. I will remove the redundancy from the tokenization for now but will want to come back to this to see if more can be removed safely. This may be a great way to reduce fees for the contracts by removing all redundant validation logic.

# QSP-11 Lack of Documentation

I have been attempting to add as much documentation to the code as possible. This may be an over time addition to the repo but I am trying.

# QSP-12 Importance of Secure Key Management in the Project

A response is written to this inside the docs folder.

# QSP-13 Validation Allows Multiple Starter Tokens on UTXO

The validation logic now checks if the value of the validating value of the starter token is exactly 1 instead of checking if the validating value is greater than the singleton value of starter token.

The old method:
```hs
(traceIfFalse "Invalid Starter Tkn" $ Value.geq validatingValue starterValue)                   -- Must contain the starter token
```

The new method:
```hs
(traceIfFalse "Invalid Starter Tkn" $ Value.valueOf validatingValue starterPid starterTkn == 1) -- Must contain the starter token
```

The new validation logic will insure that one catalog should only have one starter token.

# QSP-14 Datum Property cdtTokenizedPid May Cause Confusion

The introduction of script parameterization has made the `cdtTokenizedPid` datum value in the fractionalization redundant. The datum value has been removed and the scripts have been updated.

# QSP-15 Token Name Collision Even with Unique Prefixes

To prevent potential token name collisions, an additional "_" is now added into the nftName.

The old method:

```hs
nftName prefix num = prefix <> UsefulFuncs.integerAsByteString num
```

This allowed this potential naming collision:
```hs
nftName "TOKEN" 13 == nftName "TOKEN1" 3
```

The new method:
```hs
nftName prefix num = prefix <> "_" <> UsefulFuncs.integerAsByteString num
```

This prevents the naming collision:
```hs
nftName "TOKEN" 13 /= nftName "TOKEN1" 3
-- TOKEN_13 /= TOKEN1_3
```

# QSP-16 Unlock Operation Does Not Validate NFT Transfer

A new line of logic has been added to the unlock validation inside LockTokenizedNFTContract.

```hs
(traceIfFalse "Invalid Tkn Error" $ Value.valueOf validatingValue tPid (cdtTokenizedTn datum) == 1)
```

Now the validating value inside the unlock function must have the value of the tokenized NFT.