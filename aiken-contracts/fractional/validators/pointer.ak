use aiken/transaction.{Mint, ScriptContext}
use aiken/transaction/credential
use aiken/transaction/value
use assist/data
use assist/find
use assist/minting
use assist/signing
use assist/values
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
    // use the pointer prefix 555
    let pointer_tkn =
      values.unique_token_name(first_tx_hash, first_index, types.prefix_555)
    let mint =
      tx.mint
        |> value.from_minted_value()
        |> value.flatten()
    // the job that signs this, 
    let signer = types.find_worker(ref_datum.hot_key, types.pointer_key)
    // let signer = ref_datum.hot_key
    when context.purpose is {
      // Mint 1 ref token and 100M fractions
      Mint(own_currency_symbol) ->
        when redeemer is {
          MintTokens -> and {
              // hot key must sign
              signing.verify_sig(tx.extra_signatories, signer),
              // prevent roll over double nft attack
              first_index < 256,
              // must mint 1 pointer token
              minting.exact(mint, own_currency_symbol, pointer_tkn, 1),
              // data reference must be holding correct token
              ref_value_check,
            }
          BurnTokens { burn_amt } -> and {
              burn_amt == 0,
              // must burn 1 pointer token
              minting.by_prefix(
                mint,
                own_currency_symbol,
                // non standard prefix
                types.prefix_555,
                -1,
              )?,
              // data reference must be holding correct token
              ref_value_check?,
            }
        }
      // Minting Only
      _ -> False
    }
  }
}
