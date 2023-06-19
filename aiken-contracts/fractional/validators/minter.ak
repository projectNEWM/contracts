use aiken/list
use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/credential
use aiken/transaction/value
use assist/data
use assist/find
use assist/prefixes
use assist/signing
use assist/values
use fractional/mint_validators
use fractional/types.{BurnTokens, MintRedeemer, MintTokens, ReferenceDatum}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(redeemer: MintRedeemer, context: ScriptContext) -> Bool {
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
    // get tx info
    let first_index = find.first_input_index(tx.inputs)
    let first_tx_hash = find.first_input_txid(tx.inputs)
    // use the ref prefix
    let reference_tkn =
      values.unique_token_name(first_tx_hash, first_index, prefixes.prefix_100)
    // use the sft prefix
    let fractions_tkn =
      values.unique_token_name(first_tx_hash, first_index, prefixes.prefix_444)
    let mint =
      tx.mint
        |> value.from_minted_value()
        |> value.flatten()
    // the job that signs this, 
    let signer = types.find_worker(ref_datum.hot_key, types.minter_key)
    when context.purpose is {
      // Mint 1 ref token and 100M fractions
      Mint(own_currency_symbol) ->
        when redeemer is {
          MintTokens ->
            list.and(
              [
                // hot key must sign
                signing.verify_sig(tx.extra_signatories, signer),
                // prevent roll over double nft attack
                first_index < 256,
                // must mint 1 reference token
                mint_validators.correct_mint(
                  mint,
                  own_currency_symbol,
                  reference_tkn,
                  1,
                ),
                // must mint 100 M fractions
                mint_validators.correct_mint(
                  mint,
                  own_currency_symbol,
                  fractions_tkn,
                  100_000_000,
                ),
                // data reference must be holding correct token
                ref_value_check,
              ],
            )
          BurnTokens { burn_amt } ->
            list.and(
              [
                // has to be a multisig action
                signing.verify_multisig(
                  tx.extra_signatories,
                  ref_datum.keepers.pkhs,
                  ref_datum.keepers.threshold,
                ),
                // one ref token or any amount of fractions or both
                list.or(
                  [
                    // burn only 1 ref token
                    mint_validators.correct_burn(
                      mint,
                      own_currency_symbol,
                      prefixes.prefix_100,
                      -1,
                    ),
                    // burn any amount of the fractions
                    mint_validators.correct_burn(
                      mint,
                      own_currency_symbol,
                      prefixes.prefix_444,
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
      _ -> False
    }
  }
}
