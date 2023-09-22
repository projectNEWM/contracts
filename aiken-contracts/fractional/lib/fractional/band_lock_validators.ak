use aiken/dict
use aiken/transaction.{OutputReference, Transaction}
use aiken/transaction/value.{Value}
use assist/addresses
use assist/count
use assist/find
use assist/minting
use assist/payout
use assist/signing
use fractional/types.{BandLockUpDatum, ReferenceDatum}

/// Remove item from the order book at any point at owner's expense
pub fn remove_validation(
  datum: BandLockUpDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value
  let script_addr = validating_input.output.address
  let owner_addr = addresses.create_address(datum.owner.pkh, datum.owner.sc)
  and {
    // owner must sign it
    signing.verify_sig(tx.extra_signatories, datum.owner.pkh),
    // owner must get the utxo back
    payout.exact(owner_addr, validating_value, tx.outputs),
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    // no script outputs
    count.outputs_by_addr(tx.outputs, script_addr, 0),
    // can not have a full set
    datum.full_flag == 0,
  }
}

/// Remove item from the order book at any point at owner's expense
pub fn add_member_validation(
  datum: BandLockUpDatum,
  tx: Transaction,
  output_reference: OutputReference,
  member: Value,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value |> value.merge(member)
  let script_addr = validating_input.output.address
  expect outbound_datum: BandLockUpDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  and {
    // datum cant change
    datum == outbound_datum,
    // owner must sign it
    signing.verify_sig(tx.extra_signatories, datum.owner.pkh),
    // owner must get the utxo back
    payout.exact(script_addr, validating_value, tx.outputs),
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    // no script outputs
    count.outputs_by_addr(tx.outputs, script_addr, 1),
    // assume logical entry tx, can not have a full set
    datum.full_flag == 0,
  }
}

/// Remove item from the order book at any point at owner's expense
pub fn mint_validation(
  datum: BandLockUpDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let validating_value = validating_input.output.value
  let script_addr = validating_input.output.address
  // get the list of token names from the monster pid
  let token_names =
    value.tokens(validating_value, ref_datum.batcher_info.monster_pid)
      |> dict.keys()
  expect outbound_datum: BandLockUpDatum =
    find.output_datum_by_addr(tx.outputs, script_addr)
  // check if something is being minted here
  let mint =
    tx.mint
      |> value.from_minted_value()
      |> value.flatten()
  and {
    // owner must sign it
    signing.verify_sig(tx.extra_signatories, datum.owner.pkh),
    // owner must get the utxo back
    payout.exact(script_addr, validating_value, tx.outputs),
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    // no script outputs
    count.outputs_by_addr(tx.outputs, script_addr, 1),
    // must have a full set
    types.has_tokens(token_names, ref_datum.batcher_info.monster_tkns),
    // the full flag is now set
    types.set_full_flag(datum, outbound_datum),
    // mint exactly 1 batcher token
    minting.exact(
      mint,
      ref_datum.batcher_info.batcher_pid,
      ref_datum.batcher_info.batcher_tkn,
      1,
    ),
  }
}

/// Remove item from the order book at any point at owner's expense
pub fn burn_validation(
  datum: BandLockUpDatum,
  ref_datum: ReferenceDatum,
  tx: Transaction,
  output_reference: OutputReference,
) -> Bool {
  // the input being validated
  let validating_input = find.input_by_ref(tx.inputs, output_reference)
  let script_addr = validating_input.output.address
  // check if something is being minted here
  let mint =
    tx.mint
      |> value.from_minted_value()
      |> value.flatten()
  and {
    // single script input
    count.inputs_by_addr(tx.inputs, script_addr, 1),
    // no script outputs
    count.outputs_by_addr(tx.outputs, script_addr, 0),
    // burn exactly 1 batcher token
    minting.exact(
      mint,
      ref_datum.batcher_info.batcher_pid,
      ref_datum.batcher_info.batcher_tkn,
      -1,
    ),
    // we assume logical off chain for entry, must have full set
    datum.full_flag == 1,
  }
}
