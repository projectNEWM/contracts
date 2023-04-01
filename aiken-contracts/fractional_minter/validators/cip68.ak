use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/value
use fractional_minter/helpers
use fractional_minter/types.{
  CIP68Datum, CIP68Redeemer, PublicKeyHash, Remove, Update,
}

validator(newm_key_data: Data) {
  fn cip68(
    _datum: CIP68Datum,
    redeemer: CIP68Redeemer,
    context: ScriptContext,
  ) -> Bool {
    expect newm_key: PublicKeyHash =
      newm_key_data
    let tx =
      context.transaction
    when context.purpose is {
      Spend(output_reference) -> {
        let validating_input =
          helpers.get_input_by_ref(tx.inputs, output_reference)
        let script_addr =
          validating_input.output.address
        when redeemer is {
          // removes the utxo entirely from the contract
          Remove ->
            list.and(
              [
                // owner must sign it
                helpers.must_be_signed_by(tx, newm_key),
                // single script input
                helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
                // single script output
                helpers.count_script_outputs(tx.outputs, script_addr, 0) == 0,
              ],
            )
          // updates the metadata and can increase the min ada only
          Update { lovelace } -> {
            let validating_value =
              validating_input.output.value
            let increase_value =
              value.from_lovelace(lovelace.increase)
            let cont_value =
              value.add(validating_value, increase_value)
            list.and(
              [
                // owner must sign it
                helpers.must_be_signed_by(tx, newm_key),
                // single script input
                helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
                // single script output
                helpers.count_script_outputs(tx.outputs, script_addr, 0) == 1,
                // make sure value is continuing
                helpers.find_exact_payout(script_addr, cont_value, tx.outputs),
              ],
            )
          }
        }
      }
      // anything else fails
      _ ->
        False
    }
  }
}
