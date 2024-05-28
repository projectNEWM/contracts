use aiken/transaction.{Input, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/count
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/types/cip68.{CIP68}
use assist/types/hashes.{ValidatorHash}
use assist/values
use fractional/types/reference.{ReferenceDatum}
use fractional/types/storage.{CIP68Redeemer, RemoveMeta, UpdateMeta}

validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
) {
  fn params(
    _datum: CIP68,
    redeemer: CIP68Redeemer,
    context: ScriptContext,
  ) -> Bool {
    // the transaction being validated
    let tx: Transaction = context.transaction
    // data reference stuff
    let ref_addr: Address = credential.from_script(ref_hash)
    // find the first reference input
    let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      Spend(output_reference) -> {
        let this_input: Input = find.input_by_ref(tx.inputs, output_reference)
        let this_addr: Address = this_input.output.address
        when redeemer is {
          // removes the utxo entirely from the contract
          RemoveMeta -> and {
              // must hold correct data token
              values.prove_exact_nft(
                ref_input.output.value,
                starter_pid,
                starter_tkn,
              )?,
              // hot key must sign it
              signing.verify_sig(tx.extra_signatories, ref_datum.hot_key)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_addr, 1)?,
              // no script output
              count.outputs_by_addr(tx.outputs, this_addr, 0)?,
            }
          // updates the metadata and change min ada
          UpdateMeta { lovelace } -> {
            // that value can increase or decrease lovelace when required
            let that_value: Value =
              this_input.output.value
                |> value.add(value.ada_policy_id, value.ada_asset_name, lovelace)
            and {
              // must hold correct data token
              values.prove_exact_nft(
                ref_input.output.value,
                starter_pid,
                starter_tkn,
              )?,
              // hot key must sign it
              signing.verify_sig(tx.extra_signatories, ref_datum.hot_key)?,
              // single script input
              count.inputs_by_addr(tx.inputs, this_addr, 1)?,
              // single script output
              count.outputs_by_addr(tx.outputs, this_addr, 1)?,
              // make sure value is continuing properly
              payout.exact(this_addr, that_value, tx.outputs)?,
            }
          }
        }
      }
      // anything else fails
      _ -> False
    }
  }
}
