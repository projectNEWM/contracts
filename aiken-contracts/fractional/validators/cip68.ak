use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{
  CIP68Datum, CIP68Redeemer, ReferenceDatum, RemoveMeta, UpdateMeta,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    _datum: CIP68Datum,
    redeemer: CIP68Redeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx =
      context.transaction
    // data reference stuff
    expect ref_hash: ByteArray =
      ref_hash_data
    let ref_addr =
      credential.from_script(ref_hash)
    // find the first reference input
    let ref_input =
      helpers.find_script_input(tx.reference_inputs, ref_addr)
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // the reference datum
    expect ref_datum: ReferenceDatum =
      data.find_inbound_datum(ref_input)
    when context.purpose is {
      Spend(output_reference) -> {
        let validating_input =
          helpers.get_input_by_ref(tx.inputs, output_reference)
        let script_addr =
          validating_input.output.address
        when redeemer is {
          // removes the utxo entirely from the contract
          RemoveMeta ->
            list.and(
              [
                // must hold correct data token
                ref_value_check,
                // owner must sign it
                helpers.must_be_signed_by(tx, ref_datum.hot_key),
                // single script input
                helpers.count_script_inputs(tx.inputs, script_addr, 0) == 1,
                // single script output
                helpers.count_script_outputs(tx.outputs, script_addr, 0) == 0,
              ],
            )
          // updates the metadata and change min ada
          UpdateMeta { lovelace, direction } -> {
            let validating_value =
              validating_input.output.value
            let increase_value =
              value.from_lovelace(lovelace.increase)
            // set a direction for add and remove
            let cont_value =
              when direction is {
                // add
                0 ->
                  value.add(validating_value, increase_value)
                // subtract
                _ ->
                  value.add(validating_value, value.negate(increase_value))
              }
            list.and(
              [
                // must hold correct data token
                ref_value_check,
                // owner must sign it
                helpers.must_be_signed_by(tx, ref_datum.hot_key),
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
