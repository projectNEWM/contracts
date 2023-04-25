use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value
use fractional/helpers
use fractional/types.{BuyAmount, SaleDatum}

pub fn purchase_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  amount: BuyAmount,
) -> Bool {
  // the thing being spent
  let validating_input =
    helpers.get_validating_input(tx.inputs, output_reference)
  let validating_value =
    validating_input.output.value
  let script_addr =
    validating_input.output.address
  let seller_addr =
    helpers.create_address(datum.owner.pkh, datum.owner.sc)
  // the total cost value for the bundle
  let cost_value =
    value.from_asset(
      datum.cost.pid,
      datum.cost.tkn,
      amount.amt * datum.cost.amt,
    )
  // whats going back to the contract
  let bundle_value =
    value.from_asset(
      datum.bundle.pid,
      datum.bundle.tkn,
      amount.amt * datum.bundle.amt,
    )
  // the partial value cont to the contract
  let partial_value =
    value.add(validating_value, value.negate(bundle_value))
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    helpers.get_outbound_datum(tx, validating_input)
  list.and(
    [
      // single script input
      helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
      // single script output
      helpers.count_script_outputs(tx.outputs, script_addr, 0) == 1,
      // check for valid bundle size
      types.valid_bundle_size(amount),
      // the continuing utxo must have a correct datum
      datum == outbound_datum,
      // the seller gets paid what they want
      helpers.find_token_payout(seller_addr, cost_value, tx.outputs),
      // the leftover partial token gets send back to the script
      helpers.find_exact_payout(script_addr, partial_value, tx.outputs),
      // owner can not lie about how much they have
      value.quantity_of(validating_value, datum.bundle.pid, datum.bundle.tkn) >= amount.amt * datum.bundle.amt,
    ],
  )
}

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
