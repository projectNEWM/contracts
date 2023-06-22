use aiken/list
use aiken/math/rational
use aiken/transaction.{InlineDatum, Output, OutputReference, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use assist/addresses
use assist/count
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/std
// use assist/values
use fractional/types.{OrderBookDatum, OwnerInfo, ReferenceDatum, TxId}

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
      signing.verify_sig(tx.extra_signatories, datum.owner.pkh)?,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1)?,
      // single script output
      count.outputs_by_addr(tx.outputs, script_addr, 1)?,
      // make sure value is continuing
      payout.exact(script_addr, validating_value, tx.outputs)?,
      // datum check
      types.valid_order_update(datum, outbound_datum)?,
    ],
  )
}

/// Find the output with an inline datum that matches the owner and return the data.
/// If nothing is found then error. This works great for tx with a
/// single output and datum or where ordering is irrelevant.
///
/// ```aiken
/// find.find_datum_by_owner(tx.outputs)
/// ```
pub fn find_datum_by_not_owner(
  outputs: List<Output>,
  owner: OwnerInfo,
  val: Value,
  addr: Address,
) -> OrderBookDatum {
  when outputs is {
    [output, ..rest] ->
      when output.datum is {
        InlineDatum(outbound_datum) -> {
          expect datum: OrderBookDatum = outbound_datum
          if
          list.and(
            [
              datum.owner != owner,
              (output.value == val)?,
              output.address == addr,
            ],
          ){
          
            datum
          } else {
            find_datum_by_not_owner(rest, owner, val, addr)
          }
        }
        _ -> find_datum_by_not_owner(rest, owner, val, addr)
      }
    [] -> error @"No Datum Found"
  }
}

/// perform the swap
pub fn complete_validation(
  this_datum: OrderBookDatum,
  _ref_datum: ReferenceDatum,
  tx: Transaction,
  this_reference: OutputReference,
  that_txid: TxId,
) -> Bool {
  let this_input = find.input_by_ref(tx.inputs, this_reference)
  let this_value = this_input.output.value
  // this script we are current spending from
  let script_addr = this_input.output.address
  // we need to get that information fromt he other utxo
  let that_input =
    find.input_by_ref(tx.inputs, std.out_ref(that_txid.id, that_txid.idx))
  let that_value = that_input.output.value
  expect that_datum: OrderBookDatum = data.input_datum(that_input)
  let that_out_ref = that_input.output_reference
  // this should fail for bad redeemer data
  expect _: TxId = find.redeemer_by_ref(tx.redeemers, that_out_ref)
  // get the price ratio
  let average_price =
    types.ratio_average(this_datum.price.ratio, that_datum.price.ratio)
      |> types.ratio_to_rational()
  expect Some(reciprocal_average_price) = rational.reciprocal(average_price)
  // calc the amounts each will get
  let this_amt_ratio = types.value_to_ratio(this_value, this_datum.have)
  let that_amt_ratio = types.value_to_ratio(that_value, that_datum.have)
  // that value changes
  let that_get_amt =
    types.find_best(that_amt_ratio, average_price, this_amt_ratio)
  let that_get_value =
    value.from_asset(this_datum.have.pid, this_datum.have.tkn, that_get_amt)
  // this value changes
  let this_get_amt =
    types.find_best(this_amt_ratio, reciprocal_average_price, that_amt_ratio)
  let this_get_value =
    value.from_asset(that_datum.have.pid, that_datum.have.tkn, this_get_amt)
  // whats being paid to the batcher
  let the_incentive =
    value.from_asset(
      this_datum.incentive.pid,
      this_datum.incentive.tkn,
      this_datum.incentive.amt,
    )
  // add fee in last
  let that_cont_value =
    value.merge(that_value, that_get_value)
      |> value.merge(value.negate(this_get_value))
      |> value.merge(value.negate(the_incentive))
  // |> value.merge(types.split_fee_value(tx.fee))
  let that_outbound_datum =
    find_datum_by_not_owner(
      tx.outputs,
      this_datum.owner,
      that_cont_value,
      script_addr,
    )
  list.and(
    [
      // check if a swap can happen
      types.is_ok_to_swap(this_datum, that_datum)?,
      // force the correct datum to be ont he correct utxos
      (this_datum != that_outbound_datum)?,
      // that datum is correct
      (that_datum == that_outbound_datum)?,
      // cant reference yourself
      (this_reference != that_out_ref)?,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 2),
      // single script output
      count.outputs_by_addr(tx.outputs, script_addr, 2),
    ],
  )
}
