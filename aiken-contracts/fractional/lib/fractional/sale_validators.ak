use aiken/list
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value.{Value}
use assist/addresses
use assist/count
use assist/find
use assist/payout
use assist/signing
use assist/values
use fractional/mint_validators
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
      count.inputs_by_addr(tx.inputs, queue_addr, 1),
      // single script output
      count.outputs_by_addr(tx.outputs, queue_addr, 1),
      // input datum and output datum must equal
      datum == outbound_datum,
    ],
  )
}

/// Handle the updating of a sale
pub fn start_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  pointer: Value,
  ref_datum: ReferenceDatum,
) -> Bool {
  // the thing getting spent
  let validating_input =
    find.input_by_ref(tx.inputs, output_reference)
  let script_addr =
    validating_input.output.address
  let validating_value =
    validating_input.output.value
  // the sale utxo assumes self start
  let cont_value =
    value.add(validating_value, pointer) |> value.add(value.negate(tx.fee))
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  list.and(
    [
      // The transaction fee can not exceed an upperbound
      types.valid_tx_fee(tx.fee, ref_datum.fee_info.start_upper_bound),
      // prove pointer has pointer token
      values.prove_nft(ref_datum.pointer_pid, pointer)?,
      // prove validating value doesn't have pointer token
      !values.prove_nft(ref_datum.pointer_pid, validating_value)?,
      // value must go back to script
      payout.exact(script_addr, cont_value, tx.outputs)?,
      // the datum can not change
      datum == outbound_datum,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // single script output
      count.outputs_by_addr(tx.outputs, script_addr, 1),
    ],
  )
}

/// Handle the updating of a sale
pub fn extract_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  profit: Value,
  ref_datum: ReferenceDatum,
) -> Bool {
  // the thing getting spent
  let validating_input =
    find.input_by_ref(tx.inputs, output_reference)
  let script_addr =
    validating_input.output.address
  let validating_value =
    validating_input.output.value
  let cont_value =
    value.add(validating_value, value.negate(profit))
  // the datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  list.and(
    [
      // the owner must sign it
      signing.verify_sig(tx, datum.owner.pkh),
      // prove profit does not have pointer token
      !values.prove_nft(ref_datum.pointer_pid, profit),
      // prove cont value has pointer token
      values.prove_nft(ref_datum.pointer_pid, cont_value),
      // value must go back to script
      payout.exact(script_addr, cont_value, tx.outputs),
      // the datum can not change
      datum == outbound_datum,
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // single script output
      count.outputs_by_addr(tx.outputs, script_addr, 1),
    ],
  )
}

/// Remove sale back to owners wallet
pub fn remove_validation(
  datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  ref_datum: ReferenceDatum,
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
  let mint =
    tx.mint
      |> value.without_lovelace()
      |> value.flatten()
  let remove_or_burn =
    if values.prove_nft(ref_datum.pointer_pid, validating_value) == True {
      // must burn 1 pointer token
      mint_validators.correct_burn(
        mint,
        ref_datum.pointer_pid,
        types.prefix_555,
        -1,
      )
    } else {
      // if you dont hold the pointer then your sale never started
      True
    }
  list.and(
    [
      // if have pointer burn else remove
      remove_or_burn,
      // owner must sign it
      signing.verify_sig(tx, datum.owner.pkh),
      // owner must get the utxo back
      payout.exact(seller_addr, outbound_value, tx.outputs),
      // single script input
      count.inputs_by_addr(tx.inputs, script_addr, 1),
      // no script outputs
      count.outputs_by_addr(tx.outputs, script_addr, 0),
    ],
  )
}
