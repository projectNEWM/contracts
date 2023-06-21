use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use assist/data
use assist/find
use assist/values
use fractional/order_book_validators
use fractional/types.{
  CompleteOrder, OrderBookDatum, OrderBookRedeemer, ReferenceDatum, RefundOrder,
  RemoveOrder, UpdateOrder, ValidatorHash,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    datum: OrderBookDatum,
    redeemer: OrderBookRedeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx = context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash = ref_hash_data
    let ref_addr = credential.from_script(ref_hash)
    // find the first reference input
    let ref_input = find.input_by_addr(tx.reference_inputs, ref_addr)
    let ref_value_check =
      values.prove_exact_nft(starter_pid, starter_tkn, ref_input.output.value)
    // the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      Spend(output_reference) ->
        when redeemer is {
          RemoveOrder ->
            order_book_validators.remove_validation(datum, tx, output_reference)
          UpdateOrder ->
            order_book_validators.update_validation(datum, tx, output_reference)
          RefundOrder ->
            list.and(
              [
                ref_value_check,
                order_book_validators.refund_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                ),
              ],
            )
          CompleteOrder(that_utxo) ->
            list.and(
              [
                ref_value_check,
                order_book_validators.complete_validation(
                  datum,
                  ref_datum,
                  tx,
                  output_reference,
                  that_utxo,
                ),
              ],
            )
        }

      // spending only
      _ -> False
    }
  }
}
