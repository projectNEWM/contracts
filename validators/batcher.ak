use aiken/transaction.{Input, Mint, OutputReference, ScriptContext, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId}
use assist/addresses
use assist/data
use assist/find
use assist/types/hashes.{ValidatorHash}
use assist/values
use fractional/types/band.{BandLockUpRedeemer, BurnBand, MintBand}
use fractional/types/batcher.{
  BatcherTokenRedeemer, BurnBatcherToken, MintBatcherToken,
}
use fractional/types/reference.{ReferenceDatum}

validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
) {
  fn params(redeemer: BatcherTokenRedeemer, context: ScriptContext) -> Bool {
    expect Mint(_) = context.purpose
    // the transaction being validated
    let Transaction { inputs, reference_inputs, redeemers, .. } =
      context.transaction
    // data reference stuff
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(reference_inputs, ref_addr)
    // Get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // create the band lock address
    let band_addr: Address =
      addresses.create_script_address(
        ref_datum.contracts.band,
        ref_datum.contracts.stake,
      )
    // find the first band input
    let band_input: Input = find.input_by_addr(inputs, band_addr)
    // get the reference
    let band_out_ref: OutputReference = band_input.output_reference
    when redeemer is {
      // must spend 1 thing from the band contract and use the mint band redeemer
      MintBatcherToken -> {
        expect that_redeemer: BandLockUpRedeemer =
          find.redeemer_by_ref(redeemers, band_out_ref)
        let is_correct_redeemer =
          when that_redeemer is {
            // must mint the batcher and complete set tokens
            MintBand -> True
            _ -> fail @"Incorrect Band Lock Redeemer"
          }
        and {
          // data reference must be holding correct token
          values.prove_exact_nft(
            ref_input.output.value,
            starter_pid,
            starter_tkn,
          )?,
          // check that something is being spent from the band lock
          is_correct_redeemer?,
        }
      }
      // must spend 1 thing from the band contract and use the burn band redeemer
      BurnBatcherToken -> {
        expect that_redeemer: BandLockUpRedeemer =
          find.redeemer_by_ref(redeemers, band_out_ref)
        let is_correct_redeemer =
          when that_redeemer is {
            // must burn the batcher and complete set tokens
            BurnBand -> True
            _ -> fail @"Incorrect Band Lock Redeemer"
          }
        and {
          // data reference must be holding correct token
          values.prove_exact_nft(
            ref_input.output.value,
            starter_pid,
            starter_tkn,
          )?,
          // check that something is being spent from the band lock
          is_correct_redeemer?,
        }
      }
    }
  }
}
