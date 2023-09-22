use aiken/dict
use aiken/transaction.{OutputReference, Spend, TransactionId}
use assist/find
use fractional/types.{BandLockUpRedeemer, MintBand}

test find_redeemer() {
  let redeemer_data_type: Data = Void
  let out_ref =
    OutputReference {
      transaction_id: TransactionId { hash: #"acab" },
      output_index: 0,
    }
  let redeemers =
    dict.new()
      |> dict.insert(
           key: Spend(out_ref),
           value: redeemer_data_type,
           compare: fn(_, _) { Equal },
         )
  expect result: BandLockUpRedeemer = find.redeemer_by_ref(redeemers, out_ref)
  when result is {
    MintBand -> True
    _ -> False
  }
  // result == result
}
