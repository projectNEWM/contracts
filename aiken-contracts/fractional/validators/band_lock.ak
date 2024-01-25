use aiken/transaction.{Input, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use assist/data
use assist/find
use assist/values
use fractional/band_lock_validators
use fractional/types.{
  AddToBand, BandLockUpDatum, BandLockUpRedeemer, BurnBand, MintBand,
  ReferenceDatum, RemoveBand, ValidatorHash,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    datum: BandLockUpDatum,
    redeemer: BandLockUpRedeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx: Transaction = context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash = ref_hash_data
    let ref_addr: Address = credential.from_script(ref_hash)
    // find the first reference input
    let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // ref utxo must have starter token
    let is_correct_ref_utxo: Bool =
      values.prove_exact_nft(starter_pid, starter_tkn, ref_input.output.value)
    // the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      // spend only here
      Spend(output_reference) ->
        when redeemer is {
          // remove utxo back to owner if not full
          RemoveBand ->
            band_lock_validators.remove_validation(datum, tx, output_reference)
          // add some memebers to the band if not full
          AddToBand { member } ->
            band_lock_validators.add_member_validation(
              datum,
              tx,
              output_reference,
              member,
            )
          // mint nft and lock full band
          MintBand -> and {
              band_lock_validators.mint_validation(
                datum,
                ref_datum,
                tx,
                output_reference,
              ),
              is_correct_ref_utxo,
            }
          // burn nft and unlock full band back to owner
          BurnBand -> and {
              band_lock_validators.burn_validation(
                datum,
                ref_datum,
                tx,
                output_reference,
              ),
              is_correct_ref_utxo,
            }
        }
      // Anything else fails
      _ -> False
    }
  }
}
