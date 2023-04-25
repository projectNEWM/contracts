use aiken/list
use aiken/transaction.{Publish, ScriptContext, WithdrawFrom}
use aiken/transaction/certificate.{CredentialDelegation}
use aiken/transaction/credential.{Inline, ScriptCredential}
use aiken/transaction/value
use fractional/data
use fractional/helpers
use fractional/types.{Delegate, ReferenceDatum, Withdraw, WithdrawRedeemer}

validator(starter_pid: ByteArray, starter_tkn: ByteArray, ref_hash_data: Data) {
  fn params(redeemer: WithdrawRedeemer, context: ScriptContext) -> Bool {
    let tx =
      context.transaction
    // get reference utxo
    expect ref_hash: ByteArray =
      ref_hash_data
    let ref_addr =
      credential.from_script(ref_hash)
    let ref_input =
      helpers.find_script_input(tx.reference_inputs, ref_addr)
    // check if the ref input has the correct starter token
    let ref_value_check =
      value.quantity_of(ref_input.output.value, starter_pid, starter_tkn) == 1
    // get the reference datum
    expect ref_datum: ReferenceDatum =
      data.find_inbound_datum(ref_input)
    when context.purpose is {
      // handle all withdrawals
      WithdrawFrom(stake_credential) -> {
        let reward_addr =
          helpers.create_address(
            ref_datum.staking.reward_pkh,
            ref_datum.staking.reward_sc,
          )
        let reward =
          helpers.get_reward_amt(tx.withdrawals, stake_credential)
        when redeemer is {
          // can only withdraw
          Withdraw ->
            list.and(
              [
                // data reference must hold correct token
                ref_value_check,
                // reward address must be paid the exact reward
                helpers.find_exact_payout(
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
      Publish(certificate) -> {
        let pool_id =
          ref_datum.staking.pool_id
        when redeemer is {
          // only delegate to secific pool
          Delegate(sd) -> {
            let stake_credential =
              Inline(ScriptCredential(sd.stake_cred))
            let new_delegation =
              CredentialDelegation {
                delegator: stake_credential,
                delegatee: pool_id,
              }
            list.and([ref_value_check, certificate == new_delegation])
          }
          // anything else fails
          _ ->
            False
        }
      }
      // no other purposes
      _ ->
        False
    }
  }
}
