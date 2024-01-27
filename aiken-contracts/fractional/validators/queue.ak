use aiken/transaction.{Input, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use assist/data
use assist/find
use assist/values
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
      Spend(output_reference) ->
        when redeemer is {
          // a batcher can complete a purchase from the queue
          QueuePurchase -> and {
              // check if correct reference data
              is_correct_ref_utxo,
              // purchase it
              queue_validators.purchase_validation(
                datum,
                ref_datum,
                tx.inputs,
                tx.outputs,
                tx.redeemers,
                tx.fee,
                output_reference,
              )?,
            }
          // a batcher can auto refund a completed queue item
          QueueRefund -> and {
              // check if correct reference data
              is_correct_ref_utxo,
              // refund it
              queue_validators.refund_validation(
                datum,
                ref_datum,
                tx.reference_inputs,
                tx.inputs,
                tx.outputs,
                tx.fee,
                output_reference,
              )?,
            }
          // a buyer may always remove their queue item at any time
          QueueCancel ->
            // this is available at all times for queue items
            queue_validators.cancel_validation(
              datum,
              tx.inputs,
              tx.outputs,
              tx.extra_signatories,
              output_reference,
            )?
        }
      // anything else fails
      _ -> False
    }
  }
}
