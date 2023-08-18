use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use aiken/transaction/value
use assist/count
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/values
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
    let tx = context.transaction
    // data reference stuff
    expect ref_hash: ByteArray = ref_hash_data
    let ref_addr = credential.from_script(ref_hash)
    // find the first reference input
    let ref_input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // ref utxo must have starter token
    let ref_value_check =
      values.prove_exact_nft(starter_pid, starter_tkn, ref_input.output.value)
    // the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // the job that signs this, 
    // 6d65746164617461 == metadata
    // let signer = types.find_worker(ref_datum.hot_key, types.metadata_key)
    let signer = ref_datum.hot_key
    when context.purpose is {
      Spend(output_reference) -> {
        let validating_input = find.input_by_ref(tx.inputs, output_reference)
        let script_addr = validating_input.output.address
        when redeemer is {
          // removes the utxo entirely from the contract
          RemoveMeta -> and {
              // must hold correct data token
              ref_value_check,
              // owner must sign it
              signing.verify_sig(tx.extra_signatories, signer),
              // single script input
              count.inputs_by_addr(tx.inputs, script_addr, 1),
              // no script output
              count.outputs_by_addr(tx.outputs, script_addr, 0),
            }
          // updates the metadata and change min ada
          UpdateMeta { lovelace, direction } -> {
            let validating_value = validating_input.output.value
            // set a direction for add and remove
            let cont_value =
              when direction is {
                // add
                0 ->
                  value.add(
                    validating_value,
                    value.ada_policy_id,
                    value.ada_asset_name,
                    lovelace.increase,
                  )
                // subtract
                _ ->
                  value.add(
                    validating_value,
                    value.ada_policy_id,
                    value.ada_asset_name,
                    -lovelace.increase,
                  )
              }
            and {
              // must hold correct data token
              ref_value_check,
              // owner must sign it
              signing.verify_sig(tx.extra_signatories, signer),
              // single script input
              count.inputs_by_addr(tx.inputs, script_addr, 1),
              // single script output
              count.outputs_by_addr(tx.outputs, script_addr, 1),
              // make sure value is continuing
              payout.exact(script_addr, cont_value, tx.outputs),
            }
          }
        }
      }
      // anything else fails
      _ -> False
    }
  }
}
