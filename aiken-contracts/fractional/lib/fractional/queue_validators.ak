use aiken/transaction.{Input, OutputReference, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use assist/addresses
use assist/count
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/values
use fractional/types.{
  Purchase, QueueDatum, ReferenceDatum, SaleDatum, SaleRedeemer,
}

/// Remove item from queue at any point at owner's expense
pub fn cancel_validation(
  queue_datum: QueueDatum,
  tx: Transaction,
  queue_out_ref: OutputReference,
) -> Bool {
  // the input being validated
  let queue_input: Input = find.input_by_ref(tx.inputs, queue_out_ref)
  let queue_value: Value = queue_input.output.value
  let owner_addr: Address =
    addresses.create_address(queue_datum.owner.pkh, queue_datum.owner.sc)
  and {
    // owner must sign it
    signing.verify_sig(tx.extra_signatories, queue_datum.owner.pkh),
    // owner must get the utxo back
    payout.exact(owner_addr, queue_value, tx.outputs),
    // single script input
    count.inputs_by_vkh(tx.inputs, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // no script outputs
    count.outputs_by_vkh(tx.outputs, 0),
  }
}

/// Remove item from queue at any point at their expense
pub fn refund_validation(
  queue_datum: QueueDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  queue_out_ref: OutputReference,
) -> Bool {
  // the sale address
  let sale_addr: Address =
    addresses.create_script_address(
      ref_datum.contracts.sale,
      ref_datum.contracts.stake,
    )
  // get the sale input from the reference inputs
  let sale_input: Input = find.input_by_addr(tx.reference_inputs, sale_addr)
  let sale_out_ref: OutputReference = sale_input.output_reference
  // the current value on the sale utxo
  let sale_value: Value = sale_input.output.value
  // the sale datum
  expect sale_datum: SaleDatum = data.input_datum(sale_input)
  // official sale?
  let is_correct_sale_utxo: Bool =
    value.quantity_of(
      sale_value,
      ref_datum.pointer_pid,
      queue_datum.pointer_tkn,
    ) == 1
  // the total cost value for the bundle
  let total_cost_value: Value =
    values.multiply(sale_datum.cost, queue_datum.number_of_bundles)
  let cur_bundle_tkn_amt: Int =
    value.quantity_of(
      sale_value,
      queue_datum.bundle.pid,
      queue_datum.bundle.tkn,
    )
  // the input being validated
  let queue_input: Input = find.input_by_ref(tx.inputs, queue_out_ref)
  let queue_value: Value = queue_input.output.value
  // The sale datum bundle info and the queue pointer info must equal
  // A sale must exist for the refund action to take place
  // short circuit the sale check
  let is_correct_sale: Bool =
    if and {
      sale_datum.bundle.pid == queue_datum.bundle.pid,
      sale_datum.bundle.tkn == queue_datum.bundle.tkn,
    } {
      True
    } else {
      fail @"Incorrect Sale"
    }
  // check the state of the sale with a queue item
  let is_refund_allowed: Bool =
    if
    // Does the cost value exist in the validating value
    values.contains(total_cost_value, queue_value){
    
      // The validating value does contain the cost value.
      //
      // Does the current sale have enough tokens for at least 1 bundle?
      if cur_bundle_tkn_amt >= sale_datum.bundle.amt {
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
  let queue_addr: Address = queue_input.output.address
  let owner_addr: Address =
    addresses.create_address(queue_datum.owner.pkh, queue_datum.owner.sc)
  // subtract the fee from the value being spent
  // the incentive is paid to refund it back
  let outbound_value: Value =
    value.merge(queue_value, value.negate(tx.fee))
      |> value.add(
           queue_datum.incentive.pid,
           queue_datum.incentive.tkn,
           -queue_datum.incentive.amt,
         )
  and {
    // check if official sale
    is_correct_sale_utxo?,
    // check if the pointer is equal to the bundle
    is_correct_sale?,
    // check if utxo has cost and check if cant make sale
    is_refund_allowed?,
    // The transaction fee can not exceed an upperbound
    types.valid_tx_fee(tx.fee, ref_datum.fee_info.refund_queue_bound)?,
    // owner must get the utxo back
    payout.exact(owner_addr, outbound_value, tx.outputs)?,
    // single script input
    count.inputs_by_addr(tx.inputs, queue_addr, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // no script outputs
    count.outputs_by_vkh(tx.outputs, 0),
    // tx must be spending the certificate of batcher authenticity
    types.prove_existence_of_batcher(
      ref_datum.batcher_info.batcher_pid,
      ref_datum.batcher_info.batcher_tkn,
      tx.inputs,
      queue_out_ref,
      sale_out_ref,
    )?,
  }
}

/// Remove item from queue at any point at their expense
pub fn purchase_validation(
  queue_datum: QueueDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  queue_out_ref: OutputReference,
) -> Bool {
  // the sale contract address
  let sale_addr: Address =
    addresses.create_script_address(
      ref_datum.contracts.sale,
      ref_datum.contracts.stake,
    )
  // get the sale utxo, do some checks, and get some data
  let sale_input: Input = find.input_by_addr(tx.inputs, sale_addr)
  let sale_out_ref: OutputReference = sale_input.output_reference
  // The value on the sale utxo, we need this for the change calculation
  let sale_value: Value = sale_input.output.value
  // the sale datum
  expect sale_datum: SaleDatum = data.input_datum(sale_input)
  // official sale?, must hold pointer token made by newm
  let is_correct_sale_utxo: Bool =
    if
    value.quantity_of(
      sale_value,
      ref_datum.pointer_pid,
      queue_datum.pointer_tkn,
    ) == 1{
    
      True
    } else {
      fail @"Incorrect Sale"
    }
  // find the quantity of the bundle token on the sale utxo
  let cur_bundle_tkn_amt: Int =
    value.quantity_of(
      sale_value,
      queue_datum.bundle.pid,
      queue_datum.bundle.tkn,
    )
  // must have at least 1 bundle to make a sale
  let at_least_one_bundle: Bool =
    if cur_bundle_tkn_amt >= sale_datum.bundle.amt {
      True
    } else {
      fail @"Not Enough For A Bundle"
    }
  // allows for a partial bundle to be fulfilled
  let number_of_bundles: Int =
    // the amount on the sale utxo divided by the max sale amount is less
    // than what the buyer wants to get as defined in the queue datum
    if
    cur_bundle_tkn_amt / sale_datum.bundle.amt < queue_datum.number_of_bundles{
    
      // so get the left over
      cur_bundle_tkn_amt / sale_datum.bundle.amt
    } else {
      // or get what you want
      queue_datum.number_of_bundles
    }
  // get the queue utxo, get the value and do the swap
  let queue_input: Input = find.input_by_ref(tx.inputs, queue_out_ref)
  let queue_value: Value = queue_input.output.value
  // this script address we are current spending from
  let queue_addr: Address = queue_input.output.address
  // the total cost value for the bundle
  let total_cost_value: Value =
    values.multiply(sale_datum.cost, number_of_bundles)
  // since the cost is a value type, merge it, but sub the bundle
  // add cost value, remove bundle value
  let return_sale_value: Value =
    value.merge(sale_value, total_cost_value)
      |> value.add(
           sale_datum.bundle.pid,
           sale_datum.bundle.tkn,
           -number_of_bundles * sale_datum.bundle.amt,
         )
  // remove the fee, cost value, and incentive
  // add the bundle value
  let return_queue_value: Value =
    value.merge(queue_value, value.negate(total_cost_value))
      |> value.merge(value.negate(tx.fee))
      |> value.add(
           sale_datum.bundle.pid,
           sale_datum.bundle.tkn,
           number_of_bundles * sale_datum.bundle.amt,
         )
      |> value.add(
           queue_datum.incentive.pid,
           queue_datum.incentive.tkn,
           -queue_datum.incentive.amt,
         )
  // the queue_datum going back to the queue contract
  expect outbound_datum: QueueDatum =
    find.output_datum_by_addr(tx.outputs, queue_addr)
  // this should fail for bad data
  // the redeemer is now the general redeemer type that we seek out the specific sub redeemer
  expect that_redeemer: SaleRedeemer =
    find.redeemer_by_ref(tx.redeemers, sale_out_ref)
  let is_corect_redeemer: Bool =
    when that_redeemer is {
      // only the purchase redeemer is allowed
      Purchase -> True
      _ -> fail @"Incorrect Redeemer"
    }
  // the incentive is left open for whoever completes the tx to take
  and {
    // correct redeemer on the sale contract side
    is_corect_redeemer?,
    // check if official sale
    is_correct_sale_utxo?,
    // must have enough for the sale
    at_least_one_bundle?,
    // The transaction fee can not exceed an upperbound
    types.valid_tx_fee(tx.fee, ref_datum.fee_info.purchase_queue_bound)?,
    // prove existance of at least the incentive on the intermediate value
    types.prove_existence_of_incentive(queue_value, queue_datum.incentive)?,
    // valid bundle size
    types.valid_number_of_bundles(number_of_bundles, sale_datum.max_bundle_size)?,
    // just continue the queue_datum
    (queue_datum == outbound_datum)?,
    // queue gets the bundle
    payout.exact(queue_addr, return_queue_value, tx.outputs)?,
    // sale gets the cost
    payout.exact(sale_addr, return_sale_value, tx.outputs)?,
    // single sale script input
    count.inputs_by_addr(tx.inputs, sale_addr, 1)?,
    // the queue and the sale 
    count.inputs_by_datum(tx.inputs, 2)?,
    // one sale script output
    count.outputs_by_addr(tx.outputs, sale_addr, 1)?,
    count.outputs_by_datum(tx.outputs, 2)?,
    // tx must be spending the certificate of batcher authenticity
    types.prove_existence_of_batcher(
      ref_datum.batcher_info.batcher_pid,
      ref_datum.batcher_info.batcher_tkn,
      tx.inputs,
      queue_out_ref,
      sale_out_ref,
    )?,
  }
}
