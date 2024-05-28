use aiken/dict
use aiken/transaction.{Input, ScriptContext, Spend, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/addresses
use assist/count
use assist/data
use assist/find
use assist/minting
use assist/payout
use assist/signing
use assist/types/hashes.{ValidatorHash}
use assist/types/token
use assist/values
use fractional/types/band.{
  AddToBand, BandLockUpDatum, BandLockUpRedeemer, BurnBand, MintBand, RemoveBand,
}
use fractional/types/constants
use fractional/types/reference.{ReferenceDatum}

validator(ref_hash: ValidatorHash) {
  fn params(
    this_datum: BandLockUpDatum,
    redeemer: BandLockUpRedeemer,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      // spend only here
      Spend(output_reference) -> {
        // the transaction being validated
        let tx: Transaction = context.transaction
        // the input being validated
        let this_input: Input = find.input_by_ref(tx.inputs, output_reference)
        let this_value: Value = this_input.output.value
        // data reference stuff
        let ref_addr: Address = credential.from_script(ref_hash)
        // find the first reference input
        let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
        // the reference this_datum
        expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
        when redeemer is {
          // remove utxo back to owner if not full
          RemoveBand -> {
            let owner_addr: Address = addresses.from_wallet(this_datum.owner)
            and {
              // owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // owner must get the utxo back
              payout.exact(owner_addr, this_value, tx.outputs)?,
              // single script input
              count.inputs_by_vkh(tx.inputs, 1)?,
              // no script outputs
              count.outputs_by_vkh(tx.outputs, 0)?,
              // anything but a complete set can be removed
              !values.prove_exact_nft(
                this_value,
                ref_datum.batcher_data.batcher_pid,
                constants.complete_set_token_name,
              )?,
            }
          }
          // add some memebers to the band if not full
          AddToBand { members } -> {
            let that_value: Value =
              this_value |> token.add_tokens_to_value(members)
            let this_addr: Address = this_input.output.address
            expect that_datum: BandLockUpDatum =
              find.output_datum_by_addr(tx.outputs, this_addr)
            and {
              // this_datum cant change
              (this_datum == that_datum)?,
              // owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // send at least the members and this_value to the address
              payout.at_least(this_addr, that_value, tx.outputs)?,
              // single script input
              count.inputs_by_vkh(tx.inputs, 1)?,
              // no script outputs
              count.outputs_by_vkh(tx.outputs, 1)?,
              // anything but a complete set can be updated
              !values.prove_exact_nft(
                this_value,
                ref_datum.batcher_data.batcher_pid,
                constants.complete_set_token_name,
              )?,
            }
          }
          // mint nft and lock full band
          MintBand -> {
            let this_addr: Address = this_input.output.address
            // get the list of token names from the monster pid
            let token_names: List<AssetName> =
              this_value
                |> value.tokens(ref_datum.batcher_data.monster_pid)
                |> dict.keys()
            expect that_datum: BandLockUpDatum =
              find.output_datum_by_addr(tx.outputs, this_addr)
            // check if something is being minted here
            let mint: List<(PolicyId, AssetName, Int)> =
              tx.mint
                |> value.from_minted_value()
                |> value.flatten()
            // the complete set nft is sent back to the contract
            let that_value: Value =
              this_value
                |> value.add(
                    ref_datum.batcher_data.batcher_pid,
                    constants.complete_set_token_name,
                    1,
                  )
            and {
              // datums cant change
              (this_datum == that_datum)?,
              // owner must sign it
              signing.verify_sig(tx.extra_signatories, this_datum.owner.pkh)?,
              // at least this value and teh complete set token sent to this address
              payout.at_least(this_addr, that_value, tx.outputs)?,
              // single script input
              count.inputs_by_vkh(tx.inputs, 1)?,
              // single script outputs
              count.outputs_by_vkh(tx.outputs, 1)?,
              // must have a full set
              band.has_correct_tokens(
                token_names,
                ref_datum.batcher_data.monster_prefixes,
              )?,
              // mint exactly 1 batcher token
              minting.exact(
                mint,
                ref_datum.batcher_data.batcher_pid,
                constants.batcher_token_name,
                1,
              )?,
              // mint exactly 1 complete set token
              minting.exact(
                mint,
                ref_datum.batcher_data.batcher_pid,
                constants.complete_set_token_name,
                1,
              )?,
            }
          }
          // burn nft and unlock full band back to owner
          BurnBand -> {
            // data reference stuff
            let ref_addr: Address = credential.from_script(ref_hash)
            // find the first reference input
            let ref_input: Input =
              find.input_by_addr(tx.reference_inputs, ref_addr)
            // the reference this_datum
            expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
            // check if something is being minted here
            let mint: List<(PolicyId, AssetName, Int)> =
              tx.mint
                |> value.from_minted_value()
                |> value.flatten()
            and {
              // single script input
              count.inputs_by_vkh(tx.inputs, 1)?,
              // no script outputs
              count.outputs_by_vkh(tx.outputs, 0)?,
              // burn exactly 1 batcher token
              minting.exact(
                mint,
                ref_datum.batcher_data.batcher_pid,
                constants.batcher_token_name,
                -1,
              )?,
              // mint exactly 1 complete set token
              minting.exact(
                mint,
                ref_datum.batcher_data.batcher_pid,
                constants.complete_set_token_name,
                -1,
              )?,
              // anything but a complete set can be removed
              values.prove_exact_nft(
                this_value,
                ref_datum.batcher_data.batcher_pid,
                constants.complete_set_token_name,
              )?,
            }
          }
        }
      }
      // Anything else fails
      _ -> False
    }
  }
}
