use aiken/bytearray
use aiken/transaction.{Input, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use assist/count
use assist/find
use assist/payout
use assist/signing
use assist/types/wallet
use fractional/types/reference.{ReferenceDatum}

// use a random string here to create many reference contracts if required
validator(_random_string: ByteArray) {
  fn params(
    this_datum: ReferenceDatum,
    _redeemer: Void,
    context: ScriptContext,
  ) -> Bool {
    expect Spend(output_reference) = context.purpose
    // tx being validated
    let Transaction { inputs, outputs, extra_signatories, .. } =
      context.transaction
    let this_input: Input = find.input_by_ref(inputs, output_reference)
    // this should allow the lovelace to change naturally with the updates
    let that_value: Value = this_input.output.value |> value.without_lovelace
    let this_addr: Address = this_input.output.address
    expect that_datum: ReferenceDatum =
      find.output_datum_by_addr(outputs, this_addr)
    //
    // That datum can only update logically and only by the keepers in this
    // datum. The value validation is such that the lovelace may change
    // but whatever non-lovelace assets must remain on the UTxO.
    //
    and {
      // send back to script but allow lovelace increase or decrease
      payout.at_least(this_addr, that_value, outputs)?,
      // single script input
      count.inputs_by_addr(inputs, this_addr, 1)?,
      // single script output
      count.outputs_by_addr(outputs, this_addr, 1)?,
      // only keepers can update the data
      signing.verify_multisig(
        extra_signatories,
        this_datum.keepers.pkhs,
        this_datum.keepers.threshold,
      )?,
      // hot key needs to be valid length
      (bytearray.length(that_datum.hot_key) == 28)?,
      // keepers are logical and valid
      reference.is_valid_keepers(that_datum.keepers)?,
      // can't have a invalid reward wallet
      wallet.is_valid(that_datum.staking.reward_wallet)?,
      // contracts either exist or dont
      reference.is_valid_contracts(that_datum.contracts)?,
    }
  }
}
