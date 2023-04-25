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
        when redeemer is {
          MintTokens ->
            list.and(
              [
                // hot key must sign
                helpers.must_be_signed_by(tx, ref_datum.hot_key),
                // prevent roll over double nft attack
                first_index < 256,
                // must mint 1 reference token
                helpers.correct_mint(
                  mint,
                  own_currency_symbol,
                  reference_tkn,
                  1,
                ),
                // must mint 100 M fractions
                helpers.correct_mint(
                  mint,
                  own_currency_symbol,
                  fractions_tkn,
                  100000000,
                ),
                // data reference must be holding correct token
                ref_value_check,
              ],
            )
          BurnTokens { burn_amt } ->
            list.and(
              [
                // has to be a multisig action
                helpers.valid_multisig(tx, ref_datum.keepers.pkhs, 0) >= ref_datum.keepers.threshold,
                // one ref token or any amount of fractions or both
                list.or(
                  [
                    // burn only 1 ref token
                    helpers.correct_burn(
                      mint,
                      own_currency_symbol,
                      types.prefix_100,
                      -1,
                    ),
                    // burn any amount of the fractions
                    helpers.correct_burn(
                      mint,
                      own_currency_symbol,
                      types.prefix_444,
                      -1 * burn_amt,
                    ),
                  ],
                ),
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
