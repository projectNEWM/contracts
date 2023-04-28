use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use fractional/helpers
use fractional/types.{ReferenceDatum, SaleDatum}

/// Handle the purchasing of a bundle(s)
pub fn purchase_validation(
  datum: SaleDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the thing being spent
  let validating_input =
    helpers.get_validating_input(tx.inputs, output_reference)
  // the thing in the queue
  let queue_addr =
    helpers.create_script_address(
      ref_datum.contracts.queue,
      ref_datum.contracts.stake,
    )
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    helpers.get_outbound_datum(tx, validating_input)
  list.and(
    [
      // single script input
      (helpers.count_script_inputs(tx.inputs, queue_addr, 0) == 1)?,
      // single script output
      (helpers.count_script_outputs(tx.outputs, queue_addr, 0) == 1)?,
      (datum == outbound_datum)?,
      helpers.must_be_signed_by(tx, ref_datum.hot_key)?,
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
    helpers.get_validating_input(tx.inputs, output_reference)
  let script_addr =
    validating_input.output.address
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    helpers.get_outbound_datum(tx, validating_input)
  list.and(
    [
      // the owner must sign it
      helpers.must_be_signed_by(tx, datum.owner.pkh),
      // the owner of the cont utxo must remain constant
      datum.owner == outbound_datum.owner,
      // single script input
      helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
      // single script output
      helpers.count_script_outputs(tx.outputs, script_addr, 0) == 1,
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
    helpers.get_validating_input(tx.inputs, output_reference)
  let validating_value =
    validating_input.output.value
  let script_addr =
    validating_input.output.address
  let seller_addr =
    helpers.create_address(datum.owner.pkh, datum.owner.sc)
  list.and(
    [
      // owner must sign it
      helpers.must_be_signed_by(tx, datum.owner.pkh),
      // owner must get the utxo back
      helpers.find_exact_payout(seller_addr, validating_value, tx.outputs),
      // single script input
      helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
      // no script outputs
      helpers.count_script_outputs(tx.outputs, script_addr, 0) == 0,
    ],
  )
}
