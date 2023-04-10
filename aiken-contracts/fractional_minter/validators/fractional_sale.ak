use aiken/transaction.{ScriptContext, Spend}
use fractional_sale/datums.{Datum}
use fractional_sale/redeemers.{Purchase, Redeemer, Remove, Update}
use fractional_sale/validators

validator {
  fn fractional_sale(
    datum: Datum,
    redeemer: Redeemer,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      Spend(output_reference) ->
        when redeemer is {
          // Buyer can purchase a bundle from the UTxO
          Purchase { buy_amt } ->
            validators.purchase_validation(
              datum,
              context.transaction,
              output_reference,
              buy_amt,
            )
          // Owner can update their UTxO
          Update ->
            validators.update_validation(
              datum,
              context.transaction,
              output_reference,
            )
          // Owner can remove their UTxO
          Remove ->
            validators.remove_validation(
              datum,
              context.transaction,
              output_reference,
            )
        }
      // Anything else fails
      _ ->
        False
    }
  }
}
