# NEWM Order Book Marketplace

The contract for the order book marketplace for NEWM on Cardano, written in Plutus V2.

## Order Creation

Each order has a datum of the form below, providing the wallet info, the have token, the want token, and the swap info.

```hs
-- wallet, have, want, slippage
data OrderBookDatum = Swap OwnerInfo TokenInfo TokenInfo SwapInfo
```

Upon order creation, the OwnerInfo will be populated with your primary public key hash and staking credential. This information controls who will get the payment and who has the right to cancel and update the order. A UTxO will always remain in the original owners control until the UTxO has been fully swapped or removed from the contract.

The have and want token information is simply the policy id, token name, and the amount of that token required for a swap.

```hs
data TokenInfo = TokenInfo
  { tiPid   :: V2.CurrencySymbol
  -- ^ The policy id.
  , tiTkn   :: V2.TokenName
  -- ^ The token name.
  , tiAmt   :: Integer
  -- ^ The amount of the token.
  }
```

For example, the TokenInfo for 1000 ADA would be ```TokenInfo "" "" 1000000000```. The contract is designed for arbitrary pairs based off the have and want TokenInfo. The have is what you have on the UTxO to swap for the want defined in the datum.

Lastly, each order has a slippage associated with it. This is the amount of deviation allowed from the have and want for a swap to occur. Essentially, a swap will occur if another order is found within the slippage range.

Creating an order is as simple as selecting how much of a token you have and selecting how much of a token you want then selecting an appropriate slippage range. Orders inside the contract will be autocompleted with the order book batcher bot.

## Order Cancellation

The contract is designed to preserve ownership until the UTxO is completely swapped. At any time, the owner of the UTxO is allowed to cancel their order by removing it from the contract and placing into their wallet. It is impossible for any third party to maliciously withdraw another users funds. Bulk removal is possible for a set of owned UTxOs via tx chaining.

## Order Updating

An order may be updated at any time while in the contract. This is useful for slippage adjustments, price changes, and increases or decreasing the amount of the token the user may wish to swap. This endpoint also allows a trading pair to be changed, allowing some partial swaps to occur on pair A then the UTxO may be updated to allow partial swaps on pair B.

## Order Matching

Anyone is allowed to match orders within the contract if and only if the two UTxOs in the swap satisfy the validation parameters for either a full swap or a partial swap. A full swap is a complete swap where another UTxO is found to be within the slippage range, resulting in both UTxOs leaving the contract to their new owners. A partial swap is a swap where one UTxO has some of the token but not the full amount, resulting in one of the UTxOs returning to the contract in an attempt to be swapped again.

### Full Swap

A full swap will only occur if and only if the have token of one UTxO is within the range of the want from another and visa versa.

For example, lets take the scenario where two users are swapping NEWM and ADA. Lets assume, one user wants to swap 10 ADA for 1 NEWM with a slippage of 10% and another wants to swap 0.99 NEWM for 9.5 ADA with a slippage of 8.3%.
```hs
a = Swap pkhA (TokenInfo "" "" 10000000) (TokenInfo "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63" "4e45574d" 1000000) (SwapInfo 10)
b = Swap pkhB (TokenInfo "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63" "4e45574d" 990000) (TokenInfo "" "" 9500000) (SwapInfo 10)
```

This is an allowed full swap because 10 ADA is within 9.5 ADA +/- 8.3% and 0.99 NEWM is within 1 NEWM +/- 10%. On both sides of the swap, each UTxO is within the correct range, resulting in a full swap where pkhB gets UTxO a and pkhA gets UTxO b. The two UTxOs are removed from the contract after a full swap.

### Partial Swap

A partial swap will only occur if and only if two UTxOs lay along the same effective price line and have opposite have and want tokens within the correct range. It is very similar to the full swap but result in one of the UTxOs returning to the contract for additional swaps.

Like the full swap example, lets investigate the scenario where two users are going to complete a partial swap. Lets assume, one user wants to swap 10 ADA for 1 NEWM with a slippage of 10% and another wants to swap 0.5 NEWM for 5 ADA with a slippage of 10%.
```hs
a = Swap pkhA (TokenInfo "" "" 10000000) (TokenInfo "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63" "4e45574d" 1000000) (SwapInfo 10)
b = Swap pkhB (TokenInfo "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63" "4e45574d" 500000) (TokenInfo "" "" 5000000) (SwapInfo 10)
```

This is not a full swap because the two UTxOs are not within their respected slippage ranges but it is a partial swap because the effective price on both UTxOs lay on the same line. The effective price here is 0.1 NEWM per ADA. This validation will result in pkhA getting UTxO b and pkhB getting 5 ADA from UTxO a while returning a new UTxO to the script as shown below.

```hs
c = Swap pkhA (TokenInfo "" "" 5000000) (TokenInfo "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63" "4e45574d" 500000) (SwapInfo 10)
```

This is the returning UTxO from the user pkhA, resulting in a new potential swap with another UTxO inside the contract.

## Order Book Bot

- TODO

## Build Instructions

The cabal.project file may need to be updated for new dependencies due to on going updates.

```bash
cabal build -w ghc-8.10.7
cabal run order-book-contract
```
Or run the build-project shell script.

```bash
./build-project.sh
```
