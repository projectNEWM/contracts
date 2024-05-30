use aiken/transaction.{Input, Publish, ScriptContext, Transaction, WithdrawFrom}
use aiken/transaction/certificate.{Certificate, CredentialDelegation}
use aiken/transaction/credential.{Address, Inline, ScriptCredential}
use aiken/transaction/value.{AssetName, PolicyId}
use assist/addresses
use assist/data
use assist/find
use assist/payout
use assist/signing
use assist/types/hashes.{ValidatorHash}
use assist/values
use fractional/constants.{Lovelace}
use fractional/types/reference.{ReferenceDatum}
use fractional/types/staking.{Delegate, Withdraw, WithdrawRedeemer}

// random_string is for initilizing different stake keys attached to the same starter token
validator(
  starter_pid: PolicyId,
  starter_tkn: AssetName,
  ref_hash: ValidatorHash,
  _random_string: ByteArray,
) {
  fn params(redeemer: WithdrawRedeemer, context: ScriptContext) -> Bool {
    let Transaction {
      outputs,
      reference_inputs,
      extra_signatories,
      withdrawals,
      ..
    } = context.transaction
    // get reference utxo
    let ref_addr: Address = credential.from_script(ref_hash)
    let ref_input: Input = find.input_by_addr(reference_inputs, ref_addr)
    // get the reference datum
    expect ref_datum: ReferenceDatum = data.input_datum(ref_input)
    // the purpose can be withdrawing and publishing
    when context.purpose is {
      // handle all withdrawals
      WithdrawFrom(stake_credential) -> {
        let reward_addr: Address =
          addresses.from_wallet(ref_datum.staking.reward_wallet)
        // if no reward is found error
        let reward: Lovelace =
          find.stake_reward_by_sc(withdrawals, stake_credential)
        when redeemer is {
          // can only withdraw
          Withdraw -> and {
              // data reference must hold correct token
              values.prove_exact_nft(
                ref_input.output.value,
                starter_pid,
                starter_tkn,
              )?,
              // an at least here allows for small rewards to be withdrawn
              payout.at_least(reward_addr, value.from_lovelace(reward), outputs)?,
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
                ref_input.output.value,
                starter_pid,
                starter_tkn,
              )?,
              // The only cert is for new delegation
              (certificate == new_delegation)?,
              // only keepers can update the data
              signing.verify_multisig(
                extra_signatories,
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
