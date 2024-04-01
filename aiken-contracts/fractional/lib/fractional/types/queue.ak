use aiken/transaction/value.{AssetName, Value}
use assist/types/token.{Token}
use assist/types/wallet.{Wallet}

// single token bundle and cost
pub type QueueDatum {
  owner: Wallet,
  number_of_bundles: Int,
  incentive: Token,
  pointer_tkn: AssetName,
}

pub fn prove_existence_of_incentive(
  validating_value: Value,
  the_incentive: Token,
) -> Bool {
  and {
    // The incentive must exist
    value.quantity_of(validating_value, the_incentive.pid, the_incentive.tkn) >= the_incentive.amt,
    // the amount must be non-negative
    the_incentive.amt >= 0,
  }
}

/// Queue things
pub type QueueRedeemer {
  QueuePurchase
  QueueRefund
  QueueCancel
}
