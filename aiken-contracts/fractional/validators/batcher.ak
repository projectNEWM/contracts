use aiken/list
use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/credential
use aiken/transaction/value
use assist/addresses
use assist/count
use assist/data
use assist/find
use fractional/types.{
  BatcherTokenRedeemer, BurnBand, BurnBatcherToken, MintBand, MintBatcherToken,
  ReferenceDatum,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(redeemer: BatcherTokenRedeemer, context: ScriptContext) -> Bool {
    // the transaction being validated
    let tx = context.transaction
    // data reference stuff
    expect ref_hash: ByteArray = ref_hash_data
    let ref_addr = credential.from_script(ref_hash)
    let ref_input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // ref utxo must have starter token
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // Get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    let band_addr =
      addresses.create_script_address(
        ref_datum.contracts.band,
        ref_datum.contracts.stake,
      )
    // find the first queue input input
    let band_input = find.input_by_addr(tx.inputs, band_addr)
    let band_out_ref = band_input.output_reference

    when context.purpose is {
      // Mint 1 token
      Mint(_) ->
        when redeemer is {
          // must spend 1 thing from the band contract and use the mint band redeemer
          MintBatcherToken -> {
            expect _: MintBand =
              find.redeemer_by_ref(tx.redeemers, band_out_ref)
            list.and(
              [
                // single script input
                count.inputs_by_addr(tx.inputs, band_addr, 1),
                // single script output
                count.outputs_by_addr(tx.outputs, band_addr, 1),
                ref_value_check,
              ],
            )
          }

          // must spend 1 thing from the band contract and use the burn band redeemer
          BurnBatcherToken -> {
            expect _: BurnBand =
              find.redeemer_by_ref(tx.redeemers, band_out_ref)
            list.and(
              [
                // single script input
                count.inputs_by_addr(tx.inputs, band_addr, 1),
                // single script output
                count.outputs_by_addr(tx.outputs, band_addr, 1),
                ref_value_check,
              ],
            )
          }
        }
      // anthing else fails
      _ -> False
    }
  }
}
