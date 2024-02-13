# Entering the Initial Sale Queue

To participate in an initial sale, a user interested in purchasing bundles must join the queue, bringing sufficient incentive for the batcher and covering the gas fees. The selection of a specific sale is determined by the pointer token associated with that sale, which the batcher then uses to execute the order. The queue operates on a first-in-first-out (FIFO) basis when all incentives are equal; otherwise, a natural incentive market emerges, dictating the order fulfillment based on the level of incentive offered.

## Partial Order Fulfillment and Refunds

The system accommodates partially filled orders. In such scenarios, the batcher completes the purchase to the fullest extent possible and issues a refund for any unspent funds. Should a sale conclude with outstanding orders still in the queue, these orders are automatically refunded. Orders lacking sufficient incentive or gas are marked for removal. Users must then authorize and fund the transaction to clear these unfulfilled orders from the UTxO.

## Queue Entry and Order Validation

Entering the queue does not require validation, allowing a single transaction to contain multiple entries from one user. Each UTxO represents a distinct order. This enables users to place several maximum bundle size orders within a single sale or across multiple sales, offering flexibility and potentially enhancing the user experience.

## Incentive Mechanism

While any token can serve as an incentive, NEWM opts for the NEWM token as the preferred choice. The incentive acts as compensation for the batcher, with a complete order cycle (purchase and refund) costing 2 units of incentive. The presence of an incentive is mandatory, and it cannot be set to zero.

This system is designed to streamline the process of entering sales, providing a user-friendly approach that balances efficiency with flexibility.
