use aiken/transaction.{Input, OutputReference, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/addresses
use assist/count
use assist/find
use assist/minting
use assist/payout
use assist/signing
use assist/values
use fractional/types.{
  QueuePurchase, QueueRedeemer, ReferenceDatum, SaleDatum, TokenInfo,
}

/// Handle the purchasing of a bundle(s)
pub fn purchase_validation(
  sale_datum: SaleDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the thing being spent
  let sale_input: Input = find.input_by_ref(tx.inputs, output_reference)
  // The queue address
  let queue_addr: Address =
    addresses.create_script_address(
      ref_datum.contracts.queue,
      ref_datum.contracts.stake,
    )
  // find the first queue input input
  let queue_input: Input = find.input_by_addr(tx.inputs, queue_addr)
  let queue_out_ref: OutputReference = queue_input.output_reference
  // The sale address
  let script_addr: Address = sale_input.output.address
  // the sale_datum going back to the sale contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  // this should fail for bad data serialization
  expect that_redeemer: QueueRedeemer =
    find.redeemer_by_ref(tx.redeemers, queue_out_ref)
  let is_correct_redeemer: Bool =
    when that_redeemer is {
      QueuePurchase -> True
      _ -> fail @"Incorrect Redeemer"
    }
  // purchasing requires a constant sale_datum and 1 in /  1 out from the queue contract
  and {
    // correct redeemer on the queue side
    is_correct_redeemer?,
    // single script input
    count.inputs_by_addr(tx.inputs, queue_addr, 1)?,
    // single script output
    count.outputs_by_addr(tx.outputs, queue_addr, 1)?,
    // input sale_datum and output sale_datum must equal
    (sale_datum == outbound_datum)?,
  }
}

/// Handle the updating of a sale
pub fn start_validation(
  sale_datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  pointer: TokenInfo,
  ref_datum: ReferenceDatum,
) -> Bool {
  // the thing getting spent
  let sale_input: Input = find.input_by_ref(tx.inputs, output_reference)
  let script_addr: Address = sale_input.output.address
  let sale_value: Value = sale_input.output.value
  // the sale utxo assumes self start
  // add the pointer token and remove the fee
  let cont_value: Value =
    value.add(sale_value, pointer.pid, pointer.tkn, pointer.amt)
      |> value.merge(value.negate(tx.fee))
  let mint: List<(PolicyId, AssetName, Int)> =
    tx.mint |> value.from_minted_value() |> value.flatten()
  // the sale_datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  and {
    // The transaction fee can not exceed an upperbound
    types.valid_tx_fee(tx.fee, ref_datum.fee_info.start_sale_bound),
    // prove pointer has pointer token
    minting.is_occurring(mint, pointer.pid, pointer.tkn)?,
    // prove validating value doesn't have pointer token
    !values.prove_nft(ref_datum.pointer_pid, sale_value)?,
    // value must go back to script
    payout.exact(script_addr, cont_value, tx.outputs)?,
    // the sale_datum can not change
    sale_datum == outbound_datum,
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // single script output
    count.outputs_by_addr(tx.outputs, script_addr, 1),
    count.outputs_by_datum(tx.outputs, 1),
  }
}

/// Handle the updating of a sale
pub fn extract_validation(
  sale_datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  profit: Value,
  ref_datum: ReferenceDatum,
) -> Bool {
  // the thing getting spent
  let sale_input: Input = find.input_by_ref(tx.inputs, output_reference)
  let script_addr: Address = sale_input.output.address
  let sale_value: Value = sale_input.output.value
  let cont_value: Value = value.merge(sale_value, value.negate(profit))
  // the sale_datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  and {
    // the owner must sign it
    signing.verify_sig(tx.extra_signatories, sale_datum.owner.pkh),
    // prove profit does not have pointer token
    !values.prove_nft(ref_datum.pointer_pid, profit),
    // prove cont value has pointer token
    values.prove_nft(ref_datum.pointer_pid, cont_value),
    // value must go back to script
    payout.exact(script_addr, cont_value, tx.outputs),
    // the sale_datum can not change
    sale_datum == outbound_datum,
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // single script output
    count.outputs_by_addr(tx.outputs, script_addr, 1),
    count.outputs_by_datum(tx.outputs, 1),
  }
}

/// Remove sale back to owners wallet
pub fn remove_validation(
  sale_datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  ref_datum: ReferenceDatum,
) -> Bool {
  // the input being validated
  let sale_input: Input = find.input_by_ref(tx.inputs, output_reference)
  let sale_value: Value = sale_input.output.value
  let script_addr: Address = sale_input.output.address
  let mint: List<(PolicyId, AssetName, Int)> =
    tx.mint |> value.from_minted_value() |> value.flatten()
  // check if the pointer token needs to be burned or not
  let remove_or_burn: Bool =
    if values.prove_nft(ref_datum.pointer_pid, sale_value) {
      // must burn 1 pointer token
      minting.by_prefix(mint, ref_datum.pointer_pid, types.pointer_prefix, -1)?
    } else {
      // if you dont hold the pointer then your sale never started
      True
    }
  and {
    // if have pointer burn else remove
    remove_or_burn,
    // owner must sign it
    signing.verify_sig(tx.extra_signatories, sale_datum.owner.pkh),
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // no script outputs
    count.outputs_by_addr(tx.outputs, script_addr, 0),
  }
}

// Update the bundle size and price
pub fn update_validation(
  sale_datum: SaleDatum,
  tx: Transaction,
  output_reference: OutputReference,
  lovelace: Int,
) -> Bool {
  // the input being validated
  let sale_input: Input = find.input_by_ref(tx.inputs, output_reference)
  let sale_value: Value = sale_input.output.value
  let cont_value: Value =
    value.add(sale_value, value.ada_policy_id, value.ada_asset_name, lovelace)
  // this is the sale script
  let script_addr: Address = sale_input.output.address
  // the sale_datum going back to the contract
  expect outbound_datum: SaleDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  and {
    // the owner must sign it
    signing.verify_sig(tx.extra_signatories, sale_datum.owner.pkh),
    // value must go back to script
    payout.exact(script_addr, cont_value, tx.outputs)?,
    // the sale_datum can not change
    types.valid_sale_datum_update(sale_datum, outbound_datum)?,
    // single script input
    count.inputs_by_vkh(tx.inputs, 1),
    count.inputs_by_datum(tx.inputs, 1),
    // single script output
    count.outputs_by_vkh(tx.outputs, 1),
    count.outputs_by_datum(tx.outputs, 1),
  }
}
