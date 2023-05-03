use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/sale_validators
use fractional/types.{
  Purchase, ReferenceDatum, RemoveSale, SaleDatum, SaleRedeemer, UpdateSale,
  ValidatorHash,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    datum: SaleDatum,
    redeemer: SaleRedeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx =
      context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash =
      ref_hash_data
    let ref_addr =
      credential.from_script(ref_hash)
    // find the first reference input
    let ref_input =
      helpers.find_script_input(tx.reference_inputs, ref_addr)
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // the reference datum
    expect ref_datum: ReferenceDatum =
      data.find_inbound_datum(ref_input)
    when context.purpose is {
      // @see lib/fractional/sale_validators.ak
      Spend(output_reference) ->
        when redeemer is {
          // Buyer can purchase a bundle from the UTxO
          Purchase ->
            list.and(
              [
                ref_value_check,
                sale_validators.purchase_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
              ],
            )

          // Owner can update their UTxO
          UpdateSale ->
            sale_validators.update_validation(datum, tx, output_reference)
          // Owner can remove their UTxO
          RemoveSale ->
            sale_validators.remove_validation(datum, tx, output_reference)
        }
      // spending only
      _ ->
        False
    }
  }
}
