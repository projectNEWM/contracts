use aiken/transaction.{Input, Mint, ScriptContext, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId}
use assist/data
use assist/find
use assist/minting
use assist/signing
use assist/types/cip68
use assist/types/hashes.{TxHash, ValidatorHash}
use assist/values
use fractional/types/minter.{BurnTokens, MintRedeemer, MintTokens}
use fractional/types/reference.{ReferenceDatum}

validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
) {
  fn params(redeemer: MintRedeemer, context: ScriptContext) -> Bool {
    expect Mint(currency_symbol) = context.purpose
    // the transaction being validated
    let Transaction { inputs, reference_inputs, extra_signatories, mint, .. } =
      context.transaction
    // data reference stuff
    // assume the reference address is not staked
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(reference_inputs, ref_addr)
    // Get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // get tx info
    let first_index: Int = find.first_input_index(inputs)
    let first_tx_hash: TxHash = find.first_input_txid(inputs)
    // use the ref prefix
    let reference_tkn: AssetName =
      values.unique_token_name(first_tx_hash, first_index, cip68.prefix_100)
    // use the rft prefix
    let fractions_tkn: AssetName =
      values.unique_token_name(first_tx_hash, first_index, cip68.prefix_444)
    let mint_list: List<(PolicyId, AssetName, Int)> =
      mint
        |> value.from_minted_value()
        |> value.flatten()
    when redeemer is {
      MintTokens -> and {
          // hot key must sign
          signing.verify_sig(extra_signatories, ref_datum.hot_key)?,
          // prevent roll over double nft attack
          (first_index < 256)?,
          // must mint 1 reference token
          minting.exact(mint_list, currency_symbol, reference_tkn, 1)?,
          // must mint 100 M fractions
          minting.exact(mint_list, currency_symbol, fractions_tkn, 100_000_000)?,
          // data reference must be holding correct token
          values.prove_exact_nft(
            ref_input.output.value,
            starter_pid,
            starter_tkn,
          )?,
        }
      BurnTokens { burn_amt } -> and {
          // burn amount must be negative or zero
          (burn_amt <= 0)?,
          // has to be a multisig action
          signing.verify_multisig(
            extra_signatories,
            ref_datum.keepers.pkhs,
            ref_datum.keepers.threshold,
          )?,
          // one ref token or any amount of fractions or both
          or {
            // burn only 1 ref token
            minting.by_prefix(mint_list, currency_symbol, cip68.prefix_100, -1)?,
            // burn any amount of the fractions
            minting.by_prefix(
              mint_list,
              currency_symbol,
              cip68.prefix_444,
              burn_amt,
            )?,
          }?,
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
