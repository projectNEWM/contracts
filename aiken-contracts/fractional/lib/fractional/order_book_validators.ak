use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value
use assist/addresses
use assist/count
// use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/std
// use assist/values
use fractional/types.{OrderBookDatum, ReferenceDatum, TxId}

/// Remove item from the order book at any point at owner's expense
pub fn remove_validation(
  datum: OrderBookDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value
  let script_addr = validating_input.output.address
  let seller_addr = addresses.create_address(datum.owner.pkh, datum.owner.sc)
  list.and(
    [
      // owner must sign it
      signing.verify_sig(tx.extra_signatories, datum.owner.pkh)?,
      // owner must get the utxo back
      payout.exact(seller_addr, validating_value, tx.outputs)?,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // no script outputs
      count.outputs_by_addr(tx.outputs, script_addr, 0),
    ],
  )
}

/// Remove item from order book at any point at their expense
pub fn refund_validation(
  datum: OrderBookDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value
  // ran out of incentive
  let incentive_check =
    value.quantity_of(
      validating_value,
      datum.incentive.pid,
      datum.incentive.tkn,
    ) < datum.incentive.amt
  // ran out of money to buy
  let holding_enough_check =
    value.quantity_of(validating_value, datum.have.pid, datum.have.tkn) < datum.price.ratio.numerator
  let script_addr = validating_input.output.address
  let seller_addr = addresses.create_address(datum.owner.pkh, datum.owner.sc)
  // subtract the fee from the value being spent
  let outbound_value = value.merge(validating_value, value.negate(tx.fee))
  // The refunding of a queue item is not incentivised.
  list.and(
    [
      // do you have enough stuff
      list.or([holding_enough_check, incentive_check]),
      // The transaction fee can not exceed an upperbound
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.refund_upper_bound),
      // owner must get the utxo back
      payout.exact(seller_addr, outbound_value, tx.outputs)?,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // no script outputs
      count.outputs_by_addr(tx.outputs, script_addr, 0),
    ],
  )
}

/// Update the price or incentive on an order in the order book
pub fn update_validation(
  datum: OrderBookDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value
  let script_addr = validating_input.output.address
  expect outbound_datum: OrderBookDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  list.and(
    [
      // sign to update
      signing.verify_sig(tx.extra_signatories, datum.owner.pkh),
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // single script output
      count.outputs_by_addr(tx.outputs, script_addr, 1),
      // make sure value is continuing
      payout.exact(script_addr, validating_value, tx.outputs),
      // datum check
      types.valid_order_update(datum, outbound_datum),
    ],
  )
}

/// perform the swap
pub fn swap_validation(
  _datum: OrderBookDatum,
  _ref_datum: ReferenceDatum,
  tx: Transaction,
  _this_reference: OutputReference,
  that_txid: TxId,
) -> Bool {
  // let this_input = find.input_by_ref(tx.inputs, this_reference)
  // let this_value = this_input.output.value
  // this script we are current spending from
  // let script_addr = this_input.output.address
  let that_input =
    find.input_by_ref(tx.inputs, std.out_ref(that_txid.id, that_txid.idx))
  // let that_value = that_input.output.value
  // expect that_datum: OrderBookDatum = data.input_datum(that_input)
  let that_out_ref = that_input.output_reference
  // this should fail for bad data
  expect _: TxId = find.redeemer_by_ref(tx.redeemers, that_out_ref)
  // whats being paid to the batcher
  // let the_incentive =
  //   value.from_asset(
  //     datum.incentive.pid,
  //     datum.incentive.tkn,
  //     datum.incentive.amt,
  //   )
  True
}
