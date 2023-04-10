use aiken/list
use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/credential
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{ReferenceDatum}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(_redeemer: Void, context: ScriptContext) -> Bool {
    // the transaction being validated
    let tx =
      context.transaction
    // data reference stuff
    expect ref_hash: ByteArray =
      ref_hash_data
    let ref_addr =
      credential.from_script(ref_hash)
    let ref_input =
      helpers.find_script_input(tx.reference_inputs, ref_addr)
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    expect ref_datum: ReferenceDatum =
      data.find_inbound_datum(ref_input)
    // mint stuff
    let newm_key =
      ref_datum.hot_key
    let first_index =
      helpers.get_first_index(tx.inputs)
    let first_tx_hash =
      helpers.get_first_ref(tx.inputs)
    // use the reference prefix
    let reference_tkn =
      helpers.unique_token_name(first_tx_hash, first_index, types.prefix_100)
    // use the sft prefix
    let fractions_tkn =
      helpers.unique_token_name(first_tx_hash, first_index, types.prefix_444)
    let mint =
      tx.mint
        |> value.without_lovelace()
        |> value.flatten()
    when context.purpose is {
      // Mint 1 ref token and 100M fractions
      Mint(own_currency_symbol) ->
        list.and(
          [
            helpers.must_be_signed_by(tx, newm_key)?,
            (first_index < 256)?,
            helpers.correct_mint(mint, own_currency_symbol, reference_tkn, 1)?,
            helpers.correct_mint(
              mint,
              own_currency_symbol,
              fractions_tkn,
              100000000,
            )?,
            ref_value_check?,
          ],
        )
      // Everything else fails
      _ ->
        False
    }
  }
}
