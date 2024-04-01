use aiken/transaction.{Input,
  OutputReference, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/addresses
use assist/count
use assist/data
use assist/find
use assist/minting
use assist/payout
use assist/prefixes
use assist/signing
use assist/types/hashes.{ValidatorHash}
use assist/types/token
use assist/values
use fractional/types/queue.{QueuePurchase, QueueRedeemer}
use fractional/types/reference.{ReferenceDatum}
use fractional/types/sale.{
  ExtractSale, Purchase, RemoveSale, SaleDatum, SaleRedeemer, StartSale,
  UpdateSale,
}

validator(starter_pid: PolicyId, starter_tkn: AssetName, ref_hash_data: Data) {
  fn params(
    this_datum: SaleDatum,
    redeemer: SaleRedeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx: Transaction = context.transaction
    // data reference stuff
    expect ref_hash: ValidatorHash = ref_hash_data
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      Spend(output_reference) -> {
        // the input being validated
        let this_input: Input = find.input_by_ref(tx.inputs, output_reference)
        let this_value: Value = this_input.output.value
        let this_address: Address = this_input.output.address
        when redeemer is {
          // Buyer can purchase a bundle from the UTxO
          Purchase -> {
            // The queue address
            let queue_addr: Address =
              addresses.create_script_address(
                ref_datum.contracts.queue,
                ref_datum.contracts.stake,
              )
            // find the first queue input input
            let queue_input: Input = find.input_by_addr(tx.inputs, queue_addr)
            let queue_out_ref: OutputReference = queue_input.output_reference
            // the sale_datum going back to the sale contract
            expect that_datum: SaleDatum =
              find.output_datum_by_addr(tx.outputs, this_address)
            // this should fail for bad data serialization
            expect this_redeemer: QueueRedeemer =
              find.redeemer_by_ref(tx.redeemers, queue_out_ref)
            let is_correct_redeemer: Bool =
              when this_redeemer is {
                QueuePurchase -> True
                _ -> fail @"Incorrect Queue Redeemer"
              }
            //
            //
            //
            and {
              // correct redeemer on the queue side
              is_correct_redeemer?,
              // single script input
              count.inputs_by_addr(tx.inputs, queue_addr, 1)?,
              // single script output
              count.outputs_by_addr(tx.outputs, queue_addr, 1)?,
              // input sale_datum and output sale_datum must equal
              (this_datum == that_datum)?,
              // must be the correct reference data
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
            }
          }
          // mint a pointer token to start off a sale
          StartSale { pointer } -> {
            // add the pointer token and remove the fee
            let that_value: Value =
              value.add(this_value, ref_datum.pointer, pointer.tkn, 1)
                |> value.merge(value.negate(tx.fee))
            let mint: List<(PolicyId, AssetName, Int)> =
              tx.mint |> value.from_minted_value() |> value.flatten()
            // the sale_datum going back to the contract
            expect that_datum: SaleDatum =
              find.output_datum_by_addr(tx.outputs, this_address)
            //
            //
            //
            and {
              // must be the correct reference data
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
              // The transaction fee can not exceed an upperbound
              reference.valid_tx_fee(tx.fee, ref_datum.fees.start_sale_bound)?,
              // prove pointer has pointer token
              minting.is_occurring(mint, ref_datum.pointer, pointer.tkn)?,
              // prove validating value doesn't have pointer token
              !values.prove_nft(ref_datum.pointer, this_value)?,
              // value must go back to script
              payout.exact(this_address, that_value, tx.outputs)?,
              // the sale_datum can not change
              (this_datum == that_datum)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
              // single script output
              count.outputs_by_addr(tx.outputs, this_address, 1)?,
            }
          }
          // Extract profit back to seller wallet
          ExtractSale { profit } -> {
            // the thing getting spent
            let that_value: Value = token.add_token_to_value(this_value, profit)
            // the this_datum going back to the contract
            expect that_datum: SaleDatum =
              find.output_datum_by_addr(tx.outputs, this_address)
            and {
              // the owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // prove profit does not have pointer token
              (ref_datum.pointer != profit.pid)?,
              // removing some amount only
              (profit.amt < 0)?,
              // value must go back to script
              payout.exact(this_address, that_value, tx.outputs)?,
              // the this_datum can not change
              (this_datum == that_datum)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
              // single script output
              count.outputs_by_addr(tx.outputs, this_address, 1)?,
              // must be the correct reference data
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
            }
          }
          // Owner can remove their UTxO by burning the pointer
          RemoveSale -> {
            // check if the pointer token needs to be burned or not
            let burn_pointer_token: Bool =
              if values.prove_nft(ref_datum.pointer, this_value) {
                // the input being validated
                let mint: List<(PolicyId, AssetName, Int)> =
                  tx.mint |> value.from_minted_value() |> value.flatten()
                // must burn 1 pointer token
                minting.by_prefix(
                  mint,
                  ref_datum.pointer,
                  prefixes.callable,
                  -1,
                )
              } else {
                // if you dont hold the pointer then your sale never started
                True
              }
            //
            //
            //
            and {
              // must be the correct reference data
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
              // if have pointer burn else remove
              burn_pointer_token?,
              // the owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
              // no script output
              count.outputs_by_addr(tx.outputs, this_address, 0)?,
            }
          }
          // update the size or price of a sale
          UpdateSale { lovelace } -> {
            let that_value: Value =
              value.add(
                this_value,
                value.ada_policy_id,
                value.ada_asset_name,
                lovelace,
              )
            // the this_datum going back to the contract
            expect that_datum: SaleDatum =
              find.output_datum_by_addr(tx.outputs, this_address)
            //
            //
            //
            and {
              // the owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // value must go back to script
              payout.exact(this_address, that_value, tx.outputs)?,
              // the this_datum can not change
              sale.valid_sale_datum_update(this_datum, that_datum)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_address, 1)?,
              // single script output
              count.outputs_by_addr(tx.outputs, this_address, 1)?,
            }
          }
        }
      }
      // anything else fails
      _ -> False
    }
  }
}
