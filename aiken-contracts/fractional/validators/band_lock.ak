use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
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
    let tx = context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash = ref_hash_data
    let ref_addr = credential.from_script(ref_hash)
    // find the first reference input
    let ref_input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // ref utxo must have starter token
    let ref_value_check =
      values.prove_exact_nft(starter_pid, starter_tkn, ref_input.output.value)
    // the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      // spend only here
      Spend(output_reference) ->
        when redeemer is {
          // remove utxo back to owner if not full
          RemoveBand ->
            list.and(
              [
                band_lock_validators.remove_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
                ref_value_check,
              ],
            )
          // add some memebers to the band if not full
          AddToBand { member } ->
            list.and(
              [
                band_lock_validators.add_member_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                  member,
                ),
                ref_value_check,
              ],
            )
          // mint nft and lock full band
          MintBand ->
            list.and(
              [
                band_lock_validators.mint_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
                ref_value_check,
              ],
            )
          // burn nft and unlock full band back to owner
          BurnBand ->
            list.and(
              [
                band_lock_validators.burn_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
                ref_value_check,
              ],
            )
        }
      // Anything else fails
      _ -> False
    }
  }
}
