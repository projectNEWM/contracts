# Creating an Initial Sale UTxO

An initial sale can be set to start immediately or with a delay. A sale officially starts when it is associated with a pointer token. This token is vital as it allows the queue and batcher systems to identify and interact with an ongoing sale. Without this pointer token, the sale is considered inactive, and the tokens involved are unsellable.

## Definition of a Sale

A sale involves a specified bundle of tokens being offered for a fixed price per bundle. When a user enters the queue, they specify the quantity of bundles they wish to purchase. The sale then allocates the corresponding number of tokens at the predetermined bundle price. Sellers have the flexibility to determine the size of these bundles according to their preference. For the setup we recommend (NEWM), the bundle size is typically set to one, allowing users in the queue to purchase any quantity of tokens. The price of the tokens is defined on a per-bundle basis, meaning if the bundle size is one, the price reflects the cost of an individual token.

## Seller Privileges and Limitations

Sellers have the ability to adjust the bundle size, the cost per bundle, and the maximum number of bundles available. However, they cannot change the ownership or the contents of the bundles once the sale is live. These adjustments can be made at any time, though they are at the seller's discretion and cost. Sellers also have the option to terminate the sale at any point. If the sale is associated with a pointer token, this token will be destroyed upon the sale's cancellation.

## Profit Accumulation and Withdrawal

The sale mechanism automatically accumulates profits on the sale's UTxO. Sellers can withdraw their profits at their convenience. However, it is important to note that while profits can be withdrawn, the original bundle and pointer token cannot be removed from the sale.

This setup ensures that the sale process is flexible for sellers, while also providing a structured and reliable mechanism for buyers to participate in the sale.
