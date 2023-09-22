use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/credential
use assist/data
use assist/find
use assist/values
use fractional/sale_validators
use fractional/types.{
  ExtractSale, Purchase, ReferenceDatum, RemoveSale, SaleDatum, SaleRedeemer,
  StartSale, ValidatorHash,
}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(
    datum: SaleDatum,
    redeemer: SaleRedeemer,
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
      // @see lib/fractional/sale_validators.ak
      Spend(output_reference) ->
        when redeemer is {
          // Buyer can purchase a bundle from the UTxO
          Purchase -> and {
              // must be the correct reference data
              ref_value_check,
              // purchase sale validation
              sale_validators.purchase_validation(
                datum,
                ref_datum,
                tx,
                output_reference,
              )?,
            }
          // mint a pointer token to start off a sale
          StartSale { pointer } -> and {
              // must be the correct reference data
              ref_value_check,
              // start the sale off
              sale_validators.start_validation(
                datum,
                tx,
                output_reference,
                pointer,
                ref_datum,
              )?,
            }
          // Extract profit back to seller wallet
          ExtractSale { profit } -> and {
              // must be the correct reference data
              ref_value_check,
              // extract profit from sale
              sale_validators.extract_validation(
                datum,
                tx,
                output_reference,
                profit,
                ref_datum,
              ),
            }
          // Owner can remove their UTxO by burning the pointer
          RemoveSale -> and {
              // must be the correct reference data
              ref_value_check?,
              // remove validation logic
              sale_validators.remove_validation(
                datum,
                tx,
                output_reference,
                ref_datum,
              )?,
            }
        }
      // anything else fails
      _ -> False
    }
  }
}
