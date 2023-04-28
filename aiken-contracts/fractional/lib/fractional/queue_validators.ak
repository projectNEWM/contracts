use aiken/list
use aiken/string
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/credential.{Address, StakeCredential}
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{QueueDatum, ReferenceDatum, SaleDatum}

/// Remove item from queue at any point at their expense
pub fn cancel_validation(
  datum: QueueDatum,
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

/// Remove item from queue at any point at their expense
pub fn refund_validation(
  datum: QueueDatum,
  ref_datum: ReferenceDatum,
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
  let outbound_value =
    value.add(validating_value, value.negate(tx.fee))
  let fee =
    string.from_int(value.lovelace_of(tx.fee))
  trace fee
  list.and(
    [
      // newm must sign it
      helpers.must_be_signed_by(tx, ref_datum.hot_key),
      // owner must get the utxo back
      helpers.find_exact_payout(seller_addr, outbound_value, tx.outputs),
      // single script input
      helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
      // no script outputs
      helpers.count_script_outputs(tx.outputs, script_addr, 0) == 0,
    ],
  )
}

/// Remove item from queue at any point at their expense
pub fn purchase_validation(
  datum: QueueDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let sale_addr =
    helpers.create_script_address(
      ref_datum.contracts.sale,
      ref_datum.contracts.stake,
    )
  // find the first sale input input
  let sale_input =
    helpers.find_script_input(tx.inputs, sale_addr)
  // need this for return calculation
  let sale_value =
    sale_input.output.value
  // the sale datum
  expect sale_datum: SaleDatum =
    data.find_inbound_datum(sale_input)
  let quantity_of_pointer =
    value.quantity_of(sale_value, datum.pointer.pid, datum.pointer.tkn)
  // short circuit the amount check
  let sale_value_check =
    if quantity_of_pointer >= sale_datum.bundle.amt {
      True
    } else {
      error @"not enough for a bundle"
    }
  // allows for a partial bundle to be fulfilled
  let number_of_bundles =
    if quantity_of_pointer / sale_datum.bundle.amt < datum.amt {
      quantity_of_pointer / sale_datum.bundle.amt
    } else {
      datum.amt
    }
  // this value we are currently spending
  let validating_input =
    helpers.get_validating_input(tx.inputs, output_reference)
  let validating_value =
    validating_input.output.value
  // this script we are current spending from
  let script_addr =
    validating_input.output.address
  // the total cost value for the bundle
  let cost_value =
    helpers.multiply_value(value.zero(), sale_datum.cost, number_of_bundles)
  // whats going back to the sale contract
  let bundle_value =
    value.from_asset(
      sale_datum.bundle.pid,
      sale_datum.bundle.tkn,
      number_of_bundles * sale_datum.bundle.amt,
    )
  // calc the value cont to the sale contract
  let return_sale_value =
    value.add(sale_value, value.negate(bundle_value)) |> value.add(cost_value)
  // calcu the value return to the queue contract
  // remove the fee
  let return_queue_value =
    value.add(validating_value, value.negate(tx.fee))
      |> value.add(value.negate(cost_value))
      |> value.add(bundle_value)
  // the datum going back to the contract
  expect outbound_datum: QueueDatum =
    helpers.get_outbound_datum(tx, validating_input)
  list.and(
    [
      // valid bundle size
      types.valid_bundle_size(number_of_bundles, sale_datum.max_bundle_size)?,
      // just continue the datum
      (datum == outbound_datum)?,
      // must have eenough for the sale
      sale_value_check?,
      // newm must sign it
      helpers.must_be_signed_by(tx, ref_datum.hot_key)?,
      // queue gets the bundle
      helpers.find_exact_payout(script_addr, return_queue_value, tx.outputs)?,
      // sale gets the cost and return
      helpers.find_exact_payout(sale_addr, return_sale_value, tx.outputs)?,
      // single sale script input
      (helpers.count_script_inputs(tx.inputs, sale_addr, 0) == 1)?,
      // one  sale script output
      (helpers.count_script_outputs(tx.outputs, sale_addr, 0) == 1)?,
    ],
  )
}
