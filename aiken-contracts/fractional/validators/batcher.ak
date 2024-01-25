use aiken/transaction.{Input, Mint, OutputReference, ScriptContext, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value
use assist/addresses
use assist/data
use assist/find
use assist/types/hashes.{ValidatorHash}
use fractional/types.{
  BandLockUpRedeemer, BatcherTokenRedeemer, BurnBand, BurnBatcherToken, MintBand,
  MintBatcherToken, ReferenceDatum,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(redeemer: BatcherTokenRedeemer, context: ScriptContext) -> Bool {
    // the transaction being validated
    let tx: Transaction = context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash = ref_hash_data
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // ref utxo must have starter token
    let is_correct_ref_utxo: Bool =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // Get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // create the band lock address
    let band_addr: Address =
      addresses.create_script_address(
        ref_datum.contracts.band,
        ref_datum.contracts.stake,
      )
    // find the first band input
    let band_input: Input = find.input_by_addr(tx.inputs, band_addr)
    // get the reference
    let band_out_ref: OutputReference = band_input.output_reference

    when context.purpose is {
      // Mint 1 token
      Mint(_) ->
        when redeemer is {
          // must spend 1 thing from the band contract and use the mint band redeemer
          MintBatcherToken -> {
            expect that_redeemer: BandLockUpRedeemer =
              find.redeemer_by_ref(tx.redeemers, band_out_ref)
            let is_correct_redeemer =
              when that_redeemer is {
                // must mint the batcher
                MintBand -> True
                _ -> fail @"Incorrect Redeemer"
              }
            and {
              // check if the correct reference data is being used
              is_correct_ref_utxo?,
              // check that something is being spent from the band lock
              is_correct_redeemer?,
            }
          }
          // must spend 1 thing from the band contract and use the burn band redeemer
          BurnBatcherToken -> {
            expect that_redeemer: BandLockUpRedeemer =
              find.redeemer_by_ref(tx.redeemers, band_out_ref)
            let is_correct_redeemer =
              when that_redeemer is {
                // must burn the batcher
                BurnBand -> True
                _ -> fail @"Incorrect Redeemer"
              }
            and {
              // check if the correct reference data is being used
              is_correct_ref_utxo?,
              // check that something is being spent from the band lock
              is_correct_redeemer?,
            }
          }
        }
      // anything else fails
      _ -> False
    }
  }
}
