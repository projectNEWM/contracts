use aiken/transaction.{Input, Mint, ScriptContext, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId}
use assist/data
use assist/find
use assist/minting
use assist/prefixes
use assist/signing
use assist/types/hashes.{TxHash, ValidatorHash}
use assist/values
use fractional/types/pointer.{
  BurnPointerToken, MintPointerRedeemer, MintPointerToken,
}
use fractional/types/reference.{ReferenceDatum}

validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
) {
  fn params(redeemer: MintPointerRedeemer, context: ScriptContext) -> Bool {
    expect Mint(currency_symbol) = context.purpose
    // the transaction being validated
    let Transaction { inputs, reference_inputs, extra_signatories, mint, .. } =
      context.transaction
    // data reference stuff
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(reference_inputs, ref_addr)
    // Get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // get tx info
    let first_index: Int = find.first_input_index(inputs)
    let first_tx_hash: TxHash = find.first_input_txid(inputs)
    // use the pointer prefix
    let pointer_tkn: AssetName =
      values.unique_token_name(first_tx_hash, first_index, prefixes.callable)
    let mint_list: List<(PolicyId, AssetName, Int)> =
      mint
        |> value.from_minted_value()
        |> value.flatten()
    when redeemer is {
      // mint a pointer token to start a sale
      // this can not have destination validation due to lexico cbor
      MintPointerToken -> and {
          // hot key must sign
          signing.verify_sig(extra_signatories, ref_datum.hot_key)?,
          // prevent roll over double nft attack
          (first_index < 256)?,
          // must mint 1 pointer token
          minting.exact(mint_list, currency_symbol, pointer_tkn, 1)?,
          // data reference must be holding correct token
          values.prove_exact_nft(
            ref_input.output.value,
            starter_pid,
            starter_tkn,
          )?,
        }
      // burn a single pointer token
      BurnPointerToken -> and {
          // must burn 1 pointer token
          minting.by_prefix(
            mint_list,
            currency_symbol,
            // non standard prefix
            prefixes.callable,
            -1,
          )?,
          // data reference must be holding correct token
          values.prove_exact_nft(
            ref_input.output.value,
            starter_pid,
            starter_tkn,
          )?,
        }
    }
  }
}
