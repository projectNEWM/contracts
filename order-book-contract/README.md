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

### Full Swap

### Partial Swap

## Order Book Bot

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
