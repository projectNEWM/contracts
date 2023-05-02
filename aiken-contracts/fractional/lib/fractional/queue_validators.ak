use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{QueueDatum, ReferenceDatum, SaleDatum}

/// Remove item from queue at any point at owner's expense
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
  let sale_addr =
    helpers.create_script_address(
      ref_datum.contracts.sale,
      ref_datum.contracts.stake,
    )
  let sale_input =
    helpers.find_script_input(tx.reference_inputs, sale_addr)
  // need this for return calculation
  let sale_value =
    sale_input.output.value
  // the sale datum
  expect sale_datum: SaleDatum =
    data.find_inbound_datum(sale_input)
  // the total cost value for the bundle
  let cost_value =
    helpers.multiply_value(value.zero(), sale_datum.cost, datum.amt)
  let quantity_of_pointer =
    value.quantity_of(sale_value, datum.pointer.pid, datum.pointer.tkn)
  // the input being validated
  let validating_input =
    helpers.get_validating_input(tx.inputs, output_reference)
  let validating_value =
    validating_input.output.value
  // The sale datum bundle info and the queue pointer info must equal
  // A sale must exist for the refund action to take place
  // short circuit the sale check
  let correct_sale_check =
    if
    (
      sale_datum.bundle.pid == datum.pointer.pid && sale_datum.bundle.tkn == datum.pointer.tkn
    ) == True{
    
      True
    } else {
      error @"not the correct bundle sale"
    }
  // check the state of the sale with a queue item
  let refund_value_check =
    if
    // Does the cost value exist in the validating value
    helpers.does_value_exist_in_value(
      value.flatten(cost_value),
      validating_value,
    ) == True{
    
      // The validating value does contain the cost value.
      //
      // Does the current sale have enough tokens for at least 1 bundle?
      if quantity_of_pointer >= sale_datum.bundle.amt {
        // the sale has at least one bundle
        //
        // Do Not Refund!
        False
      } else {
        // the sale doesn't have any bundles for sale
        //
        // Refund!
        True
      }
    } else {
      // The validating value does not contain the cost value.
      //
      // Refund!
      True
    }
  // the queue script address
  let script_addr =
    validating_input.output.address
  let seller_addr =
    helpers.create_address(datum.owner.pkh, datum.owner.sc)
  // subtract the fee from the value being spent
  let outbound_value =
    value.add(validating_value, value.negate(tx.fee))
  // The refunding of a queue item is not incentivised.
  list.and(
    [
      // check if the pointer is equal to the bundle
      correct_sale_check?,
      // The transaction fee can not exceed an upperbound
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.refund_upper_bound)?,
      // check if utxo has cost and check if cant make sale
      refund_value_check?,
      // owner must get the utxo back
      helpers.find_exact_payout(seller_addr, outbound_value, tx.outputs)?,
      // single script input
      (helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1)?,
      // no script outputs
      (helpers.count_script_outputs(tx.outputs, script_addr, 0) == 0)?,
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
  // the sale input being validated
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
  // whats being paid to the batcher
  let the_incentive =
    value.from_asset(
      datum.incentive.pid,
      datum.incentive.tkn,
      datum.incentive.amt,
    )
  // remove bundle
  // add cost
  let return_sale_value =
    value.add(sale_value, cost_value) |> value.add(value.negate(bundle_value))
  // remove the fee, cost value, and incentive
  // add the bundle value
  let return_queue_value =
    value.add(validating_value, bundle_value)
      |> value.add(value.negate(cost_value))
      |> value.add(value.negate(the_incentive))
      |> value.add(value.negate(tx.fee))
  // the datum going back to the queue contract
  expect outbound_datum: QueueDatum =
    helpers.get_outbound_datum(tx, validating_input)
  // the incentive is left open for whoever completes the tx to take
  list.and(
    [
      // must have enough for the sale
      sale_value_check?,
      // The transaction fee can not exceed an upperbound
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.purchase_upper_bound)?,
      // prove existance of at least the incentive on the intermediate value
      types.prove_existence_of_incentive(validating_value, datum.incentive)?,
      // valid bundle size
      types.valid_bundle_size(number_of_bundles, sale_datum.max_bundle_size)?,
      // just continue the datum
      (datum == outbound_datum)?,
      // queue gets the bundle
      helpers.find_exact_payout(script_addr, return_queue_value, tx.outputs)?,
      // sale gets the cost
      helpers.find_exact_payout(sale_addr, return_sale_value, tx.outputs)?,
      // single sale script input
      (helpers.count_script_inputs(tx.inputs, sale_addr, 0) == 1)?,
      // one sale script output
      (helpers.count_script_outputs(tx.outputs, sale_addr, 0) == 1)?,
    ],
  )
}
