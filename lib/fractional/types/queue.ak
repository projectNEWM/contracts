use aiken/transaction.{Input, OutputReference}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/types/token.{Token}
use assist/types/wallet.{Wallet}

// the queue datum
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

pub fn prove_existence_of_batcher(
  pid: PolicyId,
  tkn: AssetName,
  inputs: List<Input>,
  queue_out_ref: OutputReference,
  sale_out_ref: OutputReference,
) -> Bool {
  when inputs is {
    [input, ..rest] ->
      // cant be queue or sale and contains at least 1
      if and {
        input.output_reference != queue_out_ref,
        input.output_reference != sale_out_ref,
        value.quantity_of(input.output.value, pid, tkn) >= 1,
      } {
        True
      } else {
        prove_existence_of_batcher(pid, tkn, rest, queue_out_ref, sale_out_ref)
      }
    // nothing was found
    [] -> False
  }
}

/// Queue things
pub type QueueRedeemer {
  QueuePurchase
  QueueRefund
  QueueCancel
}
