use aiken/transaction.{Input,
  OutputReference, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/addresses
use assist/count
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/types/hashes.{ValidatorHash}
use assist/types/token.{Token}
use assist/values
use fractional/types/constants
use fractional/types/queue.{
  QueueCancel, QueueDatum, QueuePurchase, QueueRedeemer, QueueRefund,
}
use fractional/types/reference.{ReferenceDatum}
use fractional/types/sale.{Purchase, SaleDatum, SaleRedeemer}

validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
) {
  fn params(
    this_datum: QueueDatum,
    redeemer: QueueRedeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx: Transaction = context.transaction
    // the reference this_datum
    when context.purpose is {
      Spend(output_reference) -> {
        // the input being validated
        let this_input: Input = find.input_by_ref(tx.inputs, output_reference)
        let this_value: Value = this_input.output.value
        let this_address: Address = this_input.output.address
        when redeemer is {
          // a batcher can complete a purchase from the queue
          QueuePurchase -> {
            // data reference stuff
            let ref_addr: Address = credential.from_script(ref_hash)
            let ref_input: Input =
              find.input_by_addr(tx.reference_inputs, ref_addr)
            expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
            // sale stuff
            let sale_addr: Address =
              addresses.create_script_address(
                ref_datum.contracts.sale,
                ref_datum.contracts.stake,
              )
            // get the sale input from the reference inputs
            let sale_input: Input = find.input_by_addr(tx.inputs, sale_addr)
            let sale_out_ref: OutputReference = sale_input.output_reference
            // the current value on the sale utxo
            let sale_value: Value = sale_input.output.value
            expect sale_datum: SaleDatum = data.input_datum(sale_input)
            // find the quantity of the bundle token on the sale utxo
            let cur_bundle_tkn_amt: Int =
              value.quantity_of(
                sale_value,
                sale_datum.bundle.pid,
                sale_datum.bundle.tkn,
              )
            // allows for a partial bundle to be fulfilled
            let number_of_bundles: Int =
              // the amount on the sale utxo divided by the max sale amount is less
              // than what the buyer wants to get as defined in the queue datum
              if
              cur_bundle_tkn_amt / sale_datum.bundle.amt < this_datum.number_of_bundles{
              
                // so get the left over
                cur_bundle_tkn_amt / sale_datum.bundle.amt
              } else {
                // or get what you want
                this_datum.number_of_bundles
              }
            // calculate the total cost for the number of bundles
            let total_cost: Token =
              token.multiply(sale_datum.cost, number_of_bundles)
            // since the cost is a value type, merge it, but sub the bundle
            // add cost value, remove bundle value
            let return_sale_value: Value =
              token.add_token_to_value(sale_value, total_cost)
                |> value.add(
                    sale_datum.bundle.pid,
                    sale_datum.bundle.tkn,
                    -number_of_bundles * sale_datum.bundle.amt,
                  )
            // remove the fee, cost value, and incentive
            // add the bundle value
            let return_queue_value: Value =
              token.add_token_to_value(this_value, token.negative(total_cost))
                |> value.merge(value.negate(tx.fee))
                |> value.add(
                    sale_datum.bundle.pid,
                    sale_datum.bundle.tkn,
                    number_of_bundles * sale_datum.bundle.amt,
                  )
                |> value.add(
                    this_datum.incentive.pid,
                    this_datum.incentive.tkn,
                    -this_datum.incentive.amt,
                  )
            // the queue_datum going back to the queue contract
            expect that_datum: QueueDatum =
              find.output_datum_by_addr(tx.outputs, this_address)
            // this should fail for bad data
            // the redeemer is now the general redeemer type that we seek out the specific sub redeemer
            expect that_redeemer: SaleRedeemer =
              find.redeemer_by_ref(tx.redeemers, sale_out_ref)
            let is_corect_redeemer: Bool =
              when that_redeemer is {
                // only the purchase redeemer is allowed
                Purchase -> True
                _ -> fail @"Incorrect Sale Redeemer"
              }
            //
            //
            //
            and {
              // single sale script input
              count.inputs_by_addr(tx.inputs, sale_addr, 1)?,
              // one sale script output
              count.outputs_by_addr(tx.outputs, sale_addr, 1)?,
              // queue gets the bundle
              payout.exact(this_address, return_queue_value, tx.outputs)?,
              // sale gets the cost
              payout.exact(sale_addr, return_sale_value, tx.outputs)?,
              // correct redeemer on the sale contract side
              is_corect_redeemer?,
              // keep the datum constant
              (this_datum == that_datum)?,
              // prove correct sale datum
              values.prove_exact_nft(
                sale_value,
                ref_datum.pointer,
                this_datum.pointer_tkn,
              )?,
              // must have logical number of bundles
              (this_datum.number_of_bundles > 0)?,
              (sale_datum.max_bundle_size > 0)?,
              // must be non-negative incentive
              (this_datum.incentive.amt >= 0)?,
              // max bundle amount must be postiive
              (sale_datum.bundle.amt > 0)?,
              // must have at least one bundle for sale
              (cur_bundle_tkn_amt >= sale_datum.bundle.amt)?,
              // tx must be spending the certificate of batcher authenticity
              queue.prove_existence_of_batcher(
                ref_datum.batcher_data.batcher_pid,
                constants.batcher_token_name,
                tx.inputs,
                output_reference,
                sale_out_ref,
              )?,
            }
          }
          // a batcher can auto refund a completed queue item
          QueueRefund -> {
            // data reference stuff
            let ref_addr: Address = credential.from_script(ref_hash)
            let ref_input: Input =
              find.input_by_addr(tx.reference_inputs, ref_addr)
            expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
            // sale stuff
            let sale_addr: Address =
              addresses.create_script_address(
                ref_datum.contracts.sale,
                ref_datum.contracts.stake,
              )
            // get the sale input from the reference inputs
            let sale_input: Input =
              find.input_by_addr(tx.reference_inputs, sale_addr)
            let sale_out_ref: OutputReference = sale_input.output_reference
            // the current value on the sale utxo
            let sale_value: Value = sale_input.output.value
            expect sale_datum: SaleDatum = data.input_datum(sale_input)
            // calculate the total cost for the number of bundles
            let total_cost: Token =
              token.multiply(sale_datum.cost, this_datum.number_of_bundles)
            // current number of bundle tokens
            let cur_bundle_tkn_amt: Int =
              value.quantity_of(
                sale_value,
                sale_datum.bundle.pid,
                sale_datum.bundle.tkn,
              )
            // check the state of the sale with a queue item
            let is_refund_allowed: Bool =
              if
              // Does the cost exist in the this value
              value.quantity_of(this_value, total_cost.pid, total_cost.tkn) >= total_cost.amt{
              
                // Ther queue entry could pay for some amount of bundles
                //
                // Does the current sale have enough tokens for at least 1 bundle
                if cur_bundle_tkn_amt >= sale_datum.bundle.amt {
                  // the sale has at least one bundle
                  //
                  // Do Not Refund!
                  False
                } else {
                  // the sale doesn't have any bundles for sale
                  //
                  // Refund!
                  True
                }
              } else {
                // The validating value does not contain the cost value.
                //
                // Refund!
                True
              }
            // the queue script address
            let owner_addr: Address = addresses.from_wallet(this_datum.owner)
            // subtract the fee from the value being spent
            // the incentive is paid to refund it back
            let that_value: Value =
              value.merge(this_value, value.negate(tx.fee))
                |> value.add(
                    this_datum.incentive.pid,
                    this_datum.incentive.tkn,
                    -this_datum.incentive.amt,
                  )
            //
            //
            //
            and {
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
              // prove correct reference datum
              values.prove_exact_nft(
                ref_input.output.value,
                starter_pid,
                starter_tkn,
              )?,
              // prove correct sale datum
              values.prove_exact_nft(
                sale_value,
                ref_datum.pointer,
                this_datum.pointer_tkn,
              )?,
              // must have logical number of bundles
              (this_datum.number_of_bundles > 0)?,
              (sale_datum.max_bundle_size > 0)?,
              // must be non-negative incentive
              (this_datum.incentive.amt >= 0)?,
              // can the queue entry be refunded
              is_refund_allowed?,
              // owner must get the utxo back
              payout.exact(owner_addr, that_value, tx.outputs)?,
              // tx must be spending the certificate of batcher authenticity
              queue.prove_existence_of_batcher(
                ref_datum.batcher_data.batcher_pid,
                constants.batcher_token_name,
                tx.inputs,
                output_reference,
                sale_out_ref,
              )?,
            }
          }
          // a buyer may always remove their queue item at any time
          QueueCancel ->
            //
            //
            //
            and {
              // the owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
            }
        }
      }
      // anything else fails
      _ -> False
    }
  }
}
