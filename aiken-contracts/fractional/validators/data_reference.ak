use aiken/list
use aiken/transaction.{ScriptContext, Spend}
use aiken/transaction/value
use assist/count
use assist/find
use assist/payout
use assist/signing
use fractional/types.{
  ReferenceDatum, ReferenceRedeemer, UpdateBatcher, UpdateContracts,
  UpdateFeeInfo, UpdateHotKey, UpdateKeepers, UpdatePointer, UpdateStaking,
}

validator(_random_string: ByteArray) {
  fn params(
    datum: ReferenceDatum,
    redeemer: ReferenceRedeemer,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      Spend(output_reference) -> {
        // tx being validated
        let tx = context.transaction
        let validating_input = find.input_by_ref(tx.inputs, output_reference)
        let validating_value = validating_input.output.value
        // assume it has an nft and the min lovelace can be whatever
        let cont_value = value.without_lovelace(validating_value)
        let script_addr = validating_input.output.address
        // the datum cont
        expect cont_datum: ReferenceDatum =
          find.output_datum_by_addr(tx.outputs, script_addr)
        // datum validation
        let datum_cont_check =
          when redeemer is {
            // Update the hot key
            UpdateHotKey -> and {
                datum.keepers == cont_datum.keepers,
                datum.staking == cont_datum.staking,
                datum.contracts == cont_datum.contracts,
                datum.fee_info == cont_datum.fee_info,
                datum.pointer_pid == cont_datum.pointer_pid,
              }
            // Update the multisig info
            UpdateKeepers -> {
              let l = list.length(cont_datum.keepers.pkhs)
              let t = cont_datum.keepers.threshold
              and {
                datum.hot_key == cont_datum.hot_key,
                datum.staking == cont_datum.staking,
                datum.contracts == cont_datum.contracts,
                datum.fee_info == cont_datum.fee_info,
                datum.pointer_pid == cont_datum.pointer_pid,
                // logical thresholds only
                l >= t,
                // Must have at least one
                l > 0,
              }
            }
            // Update stake pool info
            UpdateStaking -> and {
                datum.hot_key == cont_datum.hot_key,
                datum.keepers == cont_datum.keepers,
                datum.contracts == cont_datum.contracts,
                datum.fee_info == cont_datum.fee_info,
                datum.pointer_pid == cont_datum.pointer_pid,
              }
            // Update contract hash info
            UpdateContracts -> and {
                datum.hot_key == cont_datum.hot_key,
                datum.keepers == cont_datum.keepers,
                datum.staking == cont_datum.staking,
                datum.fee_info == cont_datum.fee_info,
                datum.pointer_pid == cont_datum.pointer_pid,
              }
            // update the fee info
            UpdateFeeInfo -> and {
                datum.hot_key == cont_datum.hot_key,
                datum.keepers == cont_datum.keepers,
                datum.staking == cont_datum.staking,
                datum.contracts == cont_datum.contracts,
                datum.pointer_pid == cont_datum.pointer_pid,
              }
            // Update The pointer_pid info
            UpdatePointer -> and {
                datum.hot_key == cont_datum.hot_key,
                datum.keepers == cont_datum.keepers,
                datum.staking == cont_datum.staking,
                datum.contracts == cont_datum.contracts,
                datum.fee_info == cont_datum.fee_info,
              }
            // Update the batcher info
            UpdateBatcher -> and {
                datum.hot_key == cont_datum.hot_key,
                datum.keepers == cont_datum.keepers,
                datum.staking == cont_datum.staking,
                datum.contracts == cont_datum.contracts,
                datum.fee_info == cont_datum.fee_info,
                datum.pointer_pid == cont_datum.pointer_pid,
              }
          }
        // The validaiton criteria
        and {
          // send back to script but allow lovelace increase
          payout.at_least(script_addr, cont_value, tx.outputs)?,
          // single script input
          count.inputs_by_addr(tx.inputs, script_addr, 1)?,
          // single script output
          count.outputs_by_addr(tx.outputs, script_addr, 1)?,
          // only keepers can update the data
          signing.verify_multisig(
            tx.extra_signatories,
            datum.keepers.pkhs,
            datum.keepers.threshold,
          )?,
          // the cont datum check
          datum_cont_check?,
        }
      }
      // anything else fails
      _ -> False
    }
  }
}
