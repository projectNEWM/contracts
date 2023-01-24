# NEWM's Order Book Batcher

## Partial Swap Requirements For Big UTxO Sale

- Current TxId#Index of the Big UTxO
- Current datum associated with the current Big TxId#Index

Inside that datum there is the outbound payment address and the most up-to-date have and want tokens. In this test case, the slippage is set to zero.

The buyer will create a UTxO for their buy into the contract. This will hold how much ADA they are spending and the amount of the token they will receive upon successful partial swap.

- Current TxId#Index of the buyer UTxO
- Current datum associated with the current buyer TxId#Index

The partial swap transaction involves spending both the Big UTxO and the buyer UTxO. There will be a TxOut of the ADA payment going to the owner of the Big UTxO, a TxOut containing the tokens going to the buyer, and a returning TxOut back to the script with a new datum. The redeemers used inside this transaction each hold a TxOutRef data object, the TxHash and the Index. The redeemer on the Big UTxO has a TxOutRef that points to the buyer UTxO and the redeemer on the buyer UTxO has a TxOutRef that points to the Big UTxO. A partial swap that attempts to point to themselves or a non script UTxO will fail. It will only work if and only if each UTxO points to the other UTxO.

The returning datum on the Big UTxO maintains ownership and slippage but updates the have and want token information.

- Big UTxO has have and want
- Buyer UTxO has have' and want'
- The have and want' are the same token
- The want and have' are the same token

The returning have'' and want'' equal

```hs
data TokenInfo = TokenInfo
  { tiPid   :: V2.CurrencySymbol
  -- ^ The policy id.
  , tiTkn   :: V2.TokenName
  -- ^ The token name.
  , tiAmt   :: Integer
  -- ^ The amount of the token.
  }

subtractTokenInfo :: TokenInfo -> TokenInfo -> TokenInfo
subtractTokenInfo (TokenInfo pid1 tkn1 amt1) (TokenInfo pid2 tkn2 amt2)
  | pid1 == pid2 && tkn1 == tkn2 = TokenInfo pid1 tkn1 (amt1 - amt2)
  | otherwise = traceError "Cannot subtract TokenInfo with different policy id or token name"

have'' = subtractTokenInfo have want'
want'' = subtractTokenInfo want have'
```

For example, if the Big UTxO has 1000 tokens and the buyer wants 10 tokens, the returning datum will have 990 tokens. The same flow applies for the want token. This is the nibbling of the Big UTxO from the potential buyers.

tl;dr

We need the TxId#Index and the datum. The full and partial swap can technically can be built from only that information.