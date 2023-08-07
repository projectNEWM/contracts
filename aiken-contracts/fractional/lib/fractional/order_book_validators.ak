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
use fractional/types.{
  CompleteOrder, OrderBookDatum, OrderBookRedeemer, OwnerInfo, ReferenceDatum,
  TxId,
}

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
  let owner_addr = addresses.create_address(datum.owner.pkh, datum.owner.sc)
  list.and(
    [
      // owner must sign it
      signing.verify_sig(tx.extra_signatories, datum.owner.pkh),
      // owner must get the utxo back
      payout.exact(owner_addr, validating_value, tx.outputs),
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
  // ran out of money to buy,
  let holding_enough_check =
    value.quantity_of(validating_value, datum.have.pid, datum.have.tkn) < datum.price.have_threshold
  let script_addr = validating_input.output.address
  let owner_addr = addresses.create_address(datum.owner.pkh, datum.owner.sc)
  // subtract the fee from the value being spent
  let outbound_value = value.merge(validating_value, value.negate(tx.fee))
  // The refunding of a queue item is not incentivised.
  list.and(
    [
      // do you have enough stuff
      list.or([holding_enough_check, incentive_check]),
      // The transaction fee can not exceed an upperbound
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.refund_order_bound),
      // owner must get the utxo back
      payout.exact(owner_addr, outbound_value, tx.outputs)?,
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

/// Find the output with an inline datum that doesn't match the owner and return the data.
/// If nothing is found then error. 
///
/// ```aiken
/// find.find_datum_by_not_owner(tx.outputs)
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
            [datum.owner != owner, output.value == val, output.address == addr],
          ){
          
            datum
          } else {
            find_datum_by_not_owner(rest, owner, val, addr)
          }
        }
        // anything else just skip
        _ -> find_datum_by_not_owner(rest, owner, val, addr)
      }
    // if something is matched right this errors
    [] -> fail @"No Datum Found"
  }
}

/// perform the swap
/// This is what is currently being validating and that
/// is the other thing being validated.
pub fn complete_validation(
  this_datum: OrderBookDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  this_reference: OutputReference,
  that_txid: TxId,
) -> Bool {
  // this is what is being spent
  let this_input = find.input_by_ref(tx.inputs, this_reference)
  // this value on the utxo being spent
  let this_value = this_input.output.value
  // this script we are current spending from
  let this_addr = this_input.output.address
  // now we need to get that same information from the other utxo
  let that_out_ref = std.out_ref(that_txid.id, that_txid.idx)
  // this should fail for bad redeemer data
  expect that_redeemer: OrderBookRedeemer =
    find.redeemer_by_ref(tx.redeemers, that_out_ref)
  let that_redeemer_check =
    when that_redeemer is {
      // the other redeemer must be a complete order
      CompleteOrder(_) -> True
      _ -> fail @"Bad Redeemer"
    }
  let that_input = find.input_by_ref(tx.inputs, that_out_ref)
  // that value on the other utxo
  let that_value = that_input.output.value
  // that datum on the other utxo
  expect that_datum: OrderBookDatum = data.input_datum(that_input)
  // get the geometric mean of the price
  let average_price =
    types.ratio_average(this_datum.price.ratio, that_datum.price.ratio)
      |> types.ratio_to_rational()
  // get the reciprocal for the that utxo
  expect Some(reciprocal_average_price) = rational.reciprocal(average_price)
  // calc the amounts each will get
  // This is the rational represeentation of the amount of the have token for this and that
  let this_amt_rational = types.value_to_rational(this_value, this_datum.have)
  let that_amt_rational = types.value_to_rational(that_value, that_datum.have)
  // that value change by that integer
  let that_get_amt =
    types.find_best(that_amt_rational, average_price, this_amt_rational)
  // this value changes by this integer
  let this_get_amt =
    types.find_best(
      this_amt_rational,
      reciprocal_average_price,
      that_amt_rational,
    )
  // is that get amount large enough for that datum?
  let that_is_enough =
    if that_get_amt < that_datum.price.want_threshold {
      fail @"Does Not Meet That Order Threshold"
    } else {
      True
    }
  // is this get amount large enough for this datum?
  let this_is_enough =
    if this_get_amt < this_datum.price.want_threshold {
      fail @"Does Not Meet This Order Threshold"
    } else {
      True
    }
  // That value continues to the contract
  // assumes split fees
  let that_cont_value =
    value.add(
      that_value,
      this_datum.have.pid,
      this_datum.have.tkn,
      that_get_amt,
    )
      |> value.add(that_datum.have.pid, that_datum.have.tkn, -this_get_amt)
      |> value.add(
           this_datum.incentive.pid,
           this_datum.incentive.tkn,
           -this_datum.incentive.amt,
         )
      |> value.add(
           value.ada_policy_id,
           value.ada_asset_name,
           types.split_fee_value(tx.fee),
         )
  // This checks for an outbound utxo with the correct value, datum, and address all in one.
  // It will error first if nothing is found.
  let that_outbound_datum =
    find_datum_by_not_owner(
      tx.outputs,
      this_datum.owner,
      that_cont_value,
      this_addr,
    )
  list.and(
    [
      // order must meet thresholds
      list.and([this_is_enough, that_is_enough])?,
      // order must have value tx fee
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.purchase_order_bound),
      // the other redeemer must be the complete redeemer
      that_redeemer_check,
      // check if a swap can happen
      types.is_ok_to_swap(this_datum, that_datum)?,
      // that datum is correct
      that_datum == that_outbound_datum,
      // single script input
      count.inputs_by_addr(tx.inputs, this_addr, 2),
      // single script output
      count.outputs_by_addr(tx.outputs, this_addr, 2),
    ],
  )
}
