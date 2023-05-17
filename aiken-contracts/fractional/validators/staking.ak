use aiken/list
use aiken/transaction.{Publish, ScriptContext, WithdrawFrom}
use aiken/transaction/certificate.{CredentialDelegation}
use aiken/transaction/credential.{Inline, ScriptCredential}
use aiken/transaction/value
use assist/addresses
use assist/data
use assist/find
use assist/payout
use fractional/types.{Delegate, ReferenceDatum, Withdraw, WithdrawRedeemer}

// random_string is for initilizing different stake keys attached to the same starter token
validator(
  starter_pid: ByteArray,
  starter_tkn: ByteArray,
  ref_hash_data: Data,
  _random_string: ByteArray,
) {
  fn params(redeemer: WithdrawRedeemer, context: ScriptContext) -> Bool {
    let tx =
      context.transaction
    // get reference utxo
    expect ref_hash: ByteArray =
      ref_hash_data
    let ref_addr =
      credential.from_script(ref_hash)
    let ref_input =
      find.input_by_addr(tx.reference_inputs, ref_addr)
    // check if the ref input has the correct starter token
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // get the reference datum
    expect ref_datum: ReferenceDatum =
      data.input_datum(ref_input)
    when context.purpose is {
      // handle all withdrawals
      WithdrawFrom(stake_credential) -> {
        let reward_addr =
          addresses.create_address(
            ref_datum.staking.reward_pkh,
            ref_datum.staking.reward_sc,
          )
        // if no reward is found error
        let reward =
          find.stake_reward_by_sc(tx.withdrawals, stake_credential)
        when redeemer is {
          // can only withdraw
          Withdraw ->
            list.and(
              [
                // data reference must hold correct token
                ref_value_check,
                // reward address must be paid the exact reward
                payout.exact(
                  reward_addr,
                  value.from_lovelace(reward),
                  tx.outputs,
                ),
              ],
            )
          // all else fails
          _ ->
            False
        }
      }
      // handle all delegation certs
      Publish(certificate) ->
        when redeemer is {
          // only delegate to secific pool
          Delegate(sd) -> {
            let new_delegation =
              CredentialDelegation {
                delegator: Inline(ScriptCredential(sd.stake_cred)),
                delegatee: ref_datum.staking.pool_id,
              }
            list.and(
              [
                // data reference must hold correct token
                ref_value_check,
                // The only cert is for new delegation
                certificate == new_delegation,
              ],
            )
          }
          // deregistration fails and anything else but changing delegation
          _ ->
            False
        }
      // no other script purposes
      _ ->
        False
    }
  }
}
