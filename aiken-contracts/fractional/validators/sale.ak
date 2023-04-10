use aiken/transaction.{ScriptContext, Spend}
use fractional/types.{Purchase, RemoveSale, SaleDatum, SaleRedeemer, UpdateSale}
use fractional/validators

validator {
  fn sale(
    datum: SaleDatum,
    redeemer: SaleRedeemer,
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
          UpdateSale ->
            validators.update_validation(
              datum,
              context.transaction,
              output_reference,
            )
          // Owner can remove their UTxO
          RemoveSale ->
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
