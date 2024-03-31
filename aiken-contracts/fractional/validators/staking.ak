use aiken/transaction.{Input, Publish, ScriptContext, Transaction, WithdrawFrom}
use aiken/transaction/certificate.{Certificate, CredentialDelegation}
use aiken/transaction/credential.{Address, Inline, ScriptCredential}
use aiken/transaction/value.{AssetName, PolicyId}
use assist/addresses
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/values
use fractional/types/constants.{Lovelace}
use assist/types/hashes.{ValidatorHash}
use fractional/types/reference.{ReferenceDatum}
use fractional/types/staking.{Delegate, Withdraw, WithdrawRedeemer}

// random_string is for initilizing different stake keys attached to the same starter token
validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash_data: Data,
  _random_string: ByteArray,
) {
  fn params(redeemer: WithdrawRedeemer, context: ScriptContext) -> Bool {
    let tx: Transaction = context.transaction
    // get reference utxo
    expect ref_hash: ValidatorHash = ref_hash_data
    // the reference address is assumed to not be staked
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(tx.reference_inputs, ref_addr)
    // get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    when context.purpose is {
      // handle all withdrawals
      WithdrawFrom(stake_credential) -> {
        let reward_addr: Address =
          addresses.from_wallet(ref_datum.staking.reward_wallet)
        // if no reward is found error
        let reward: Lovelace =
          find.stake_reward_by_sc(tx.withdrawals, stake_credential)
        when redeemer is {
          // can only withdraw
          Withdraw -> and {
              // data reference must hold correct token
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
              // an at least here allows for small rewards to be withdrawn
              payout.at_least(
                reward_addr,
                value.from_lovelace(reward),
                tx.outputs,
              )?,
            }
          // other redeemers fail here
          _ -> False
        }
      }
      // handle all delegation certs
      Publish(certificate) ->
        when redeemer is {
          // only delegate to secific pool
          Delegate(sd) -> {
            let new_delegation: Certificate =
              CredentialDelegation {
                delegator: Inline(ScriptCredential(sd.stake_cred)),
                delegatee: ref_datum.staking.pool_id,
              }
            and {
              // data reference must hold correct token
              values.prove_exact_nft(
                starter_pid,
                starter_tkn,
                ref_input.output.value,
              )?,
              // The only cert is for new delegation
              (certificate == new_delegation)?,
              // only keepers can update the data
              signing.verify_multisig(
                tx.extra_signatories,
                ref_datum.keepers.pkhs,
                ref_datum.keepers.threshold,
              )?,
            }
          }
          // deregistration fails and anything else but changing delegation
          _ -> False
        }
      // anything else fails
      _ -> False
    }
  }
}
