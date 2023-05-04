use aiken/list
use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/credential
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{BurnTokens, MintRedeemer, MintTokens, ReferenceDatum}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(redeemer: MintRedeemer, context: ScriptContext) -> Bool {
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
    // ref utxo must have starter token
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // Get the reference datum
    expect ref_datum: ReferenceDatum =
      data.find_inbound_datum(ref_input)
    // get tx info
    let first_index =
      helpers.get_first_index(tx.inputs)
    let first_tx_hash =
      helpers.get_first_ref(tx.inputs)
    // use the ref prefix
    let pointer_tkn =
      helpers.unique_token_name(first_tx_hash, first_index, #"")
    let mint =
      tx.mint
        |> value.without_lovelace()
        |> value.flatten()
    when context.purpose is {
      // Mint 1 ref token and 100M fractions
      Mint(own_currency_symbol) ->
        when redeemer is {
          MintTokens -> {
            // the job that signs this, 
            let signer =
              types.find_worker(ref_datum.hot_key, types.pointer_key)
            list.and(
              [
                // hot key must sign
                helpers.must_be_signed_by(tx, signer),
                // prevent roll over double nft attack
                first_index < 256,
                // must mint 1 pointer token
                helpers.correct_mint(mint, own_currency_symbol, pointer_tkn, 1),
                // data reference must be holding correct token
                ref_value_check,
              ],
            )
          }
          BurnTokens { burn_amt } ->
            list.and(
              [
                burn_amt == 0,
                // must burn 1 pointer token
                helpers.correct_mint(mint, own_currency_symbol, pointer_tkn, -1),
                // data reference must be holding correct token
                ref_value_check,
              ],
            )
        }
      // Minting Only
      _ ->
        False
    }
  }
}
