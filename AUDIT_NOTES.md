# Best Practices Notes

4. I can not find any examples of value lifting for PlutusV2 at the time of audit. If an example is found then hardcoded values will be replaced with lifted values.

6. A) isNInputs will stay the same for now.

7. Code Refactor will occur after best practice 8.

8. Min/Max of script will occur after audit completion with a refactor.

10. I Combined the function and changed the name to something that reflects the functionality.

12. All cabal files are the newest I can get since IOG is not putting in effort to maintain the plutus starter repo. This is the most up-to-date cabal project file I can find that actually compiles Plutus V2 smart contracts.

### Reasoning

A lot of the best practices suggested will not change validation logic nor add additional code to the contracts. Many of them are already best practices in many of my other contracts and I been waiting for the audit to make these changes. The audit fixes will start with the implementation of best practices.