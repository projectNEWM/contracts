use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value
use assist/addresses
use assist/count
use assist/find
use assist/payout
use assist/signing
// use fractional/helpers
use fractional/types.{QueuePurchase, ReferenceDatum, SaleDatum}

/// Handle the purchasing of a bundle(s)
pub fn purchase_validation(
  datum: SaleDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the thing being spent
  let validating_input =
    find.input_by_ref(tx.inputs, output_reference)
  // the thing in the queue
  let queue_addr =
    addresses.create_script_address(
      ref_datum.contracts.queue,
      ref_datum.contracts.stake,
    )
  // find the first sale input input
  let queue_input =
    find.input_by_addr(tx.inputs, queue_addr)
  let queue_out_ref =
    queue_input.output_reference
  // the datum going back to the contract
  let script_addr =
    validating_input.output.address
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  // this should fail for bad data
  expect _: QueuePurchase =
    find.redeemer_by_ref(tx.redeemers, queue_out_ref)
  // purchasing requires a constant datum and 1 thing being spent from the queue
  list.and(
    [
      // single script input
      count.script_inputs(tx.inputs, queue_addr, 1),
      // single script output
      count.script_outputs(tx.outputs, queue_addr, 1),
      // input datum and output datum must equal
      datum == outbound_datum,
    ],
  )
}

/// Handle the updating of a sale
pub fn update_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the thing getting spent
  let validating_input =
    find.input_by_ref(tx.inputs, output_reference)
  let script_addr =
    validating_input.output.address
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  list.and(
    [
      // the owner must sign it
      signing.verify_sig(tx, datum.owner.pkh),
      // the datum can not change
      datum == outbound_datum,
      // single script input
      count.script_inputs(tx.inputs, script_addr, 1),
      // single script output
      count.script_outputs(tx.outputs, script_addr, 1),
    ],
  )
}

/// Remove sale back to owners wallet
pub fn remove_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input =
    find.input_by_ref(tx.inputs, output_reference)
  let validating_value =
    validating_input.output.value
  let burned_value =
    tx.mint
      |> value.without_lovelace()
  let outbound_value =
    value.add(validating_value, burned_value)
  let script_addr =
    validating_input.output.address
  let seller_addr =
    addresses.create_address(datum.owner.pkh, datum.owner.sc)
  list.and(
    [
      // owner must sign it
      signing.verify_sig(tx, datum.owner.pkh),
      // owner must get the utxo back
      payout.exact(seller_addr, outbound_value, tx.outputs),
      // single script input
      count.script_inputs(tx.inputs, script_addr, 1),
      // no script outputs
      count.script_outputs(tx.outputs, script_addr, 0),
    ],
  )
}
