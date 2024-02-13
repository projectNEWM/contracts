# Management of the Upper Fee Boundaries

Automated actions within the system are incentivized to ensure efficiency and security. The batcher, responsible for these actions, uses the UTxO to cover transaction fees, deducting a unit of incentive from the transaction's value. To prevent potential drain attacks by the batcher, an upper limit on fee payments is established in the reference data. This limit is crucial; without it, a batcher could fulfill an order while allocating an excessive amount of the transaction's lovelace to fees. Such actions could deplete the queue UTxO, forcing it into a removal state rather than a refund state. This would necessitate additional fees from the user to retrieve their order, benefitting the batcher without any drawback, thus making it essential to enforce an upper fee limit.

## Calculating the Upper Fee Limit

The upper fee limit is determined using the `best-case-input-worst-case-token` methodology:
- **Best Case Input**: Considers the queue UTxO, the sale UTxO, and a single batcher UTxO. Both the queue and sale contracts are linked to a stake key from the staking contract, whereas the batcher's address is assumed to be stakeless. This scenario predicts three inputs (two with base addresses and one enterprise address) and three outputs as the best case.
- **Worst Case Token**: Involves a token with the maximum allowable amount and the longest token name. The queue UTxO accounts for the worst-case scenarios in payment, incentive, and bundle, assuming the cost of a single token. The sale UTxO includes the worst-case payment and bundle scenarios.

The fee cap for performing purchase and refund actions, adhering to the `best-case-input-worst-case-token` conditions, is derived from a transaction meeting these criteria. This cap prevents exploitation of the system as a method for processing transactions without appropriate fees and ensures the fee is adequate for the data required, given the constraints of the maximum possible token parameters.

## Preventing Drain Attacks

With a carefully calculated upper limit, the risk of drain attacks is mitigated. Users can safeguard their transactions from automatically being pushed to the removal state by exceeding the upper limit by a minimal margin. This upper limit is a mandatory safeguard for each automated action, ensuring the integrity and fairness of the transaction process.
