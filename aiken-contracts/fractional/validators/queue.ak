use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use aiken/transaction/value
use assist/data
use assist/find
use fractional/queue_validators
use fractional/types.{
  QueueCancel, QueueDatum, QueuePurchase, QueueRedeemer, QueueRefund,
  ReferenceDatum, ValidatorHash,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    datum: QueueDatum,
    redeemer: QueueRedeemer,
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
      find.input_by_addr(tx.reference_inputs, ref_addr)
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // the reference datum
    expect ref_datum: ReferenceDatum =
      data.input_datum(ref_input)
    when context.purpose is {
      Spend(output_reference) ->
        when redeemer is {
          QueuePurchase ->
            list.and(
              [
                // check if correct reference data
                ref_value_check,
                // purchase it
                queue_validators.purchase_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
              ],
            )
          QueueRefund ->
            list.and(
              [
                // check if correct reference data
                ref_value_check,
                // refund it
                queue_validators.refund_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
              ],
            )
          QueueCancel ->
            queue_validators.cancel_validation(datum, tx, output_reference)
        }
      _ ->
        False
    }
  }
}
