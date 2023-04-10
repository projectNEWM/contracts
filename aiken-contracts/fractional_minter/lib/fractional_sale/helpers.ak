use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.{Input, OutputReference, Transaction}
use aiken/transaction/credential.{StakeCredential}
use fractional_sale/address
use fractional_sale/data
use fractional_sale/datums.{PublicKeyHash}

pub fn get_outbound_datum(tx: Transaction, validating_input: Input) -> Data {
  let script_addr = validating_input.output.address
  let outbound_output = address.find_script_output(tx.outputs, script_addr)
  data.find_outbound_datum(outbound_output)
}

pub fn must_be_signed_by(transaction: Transaction, vk: PublicKeyHash) -> Bool {
  list.has(transaction.extra_signatories, vk)
}

pub fn valid_multisig(
  transaction: Transaction,
  vks: List<PublicKeyHash>,
  counter: Int,
) -> Int {
  when vks is {
    [vk, ..rest] ->
      if list.has(transaction.extra_signatories, vk) == True {
        valid_multisig(transaction, rest, counter + 1)
      } else {
        valid_multisig(transaction, rest, counter)
      }
    [] -> counter
  }
}

pub fn get_validating_input(
  inputs: List<Input>,
  out_ref: OutputReference,
) -> Input {
  when inputs is {
    [input, ..rest] ->
      if input.output_reference == out_ref {
        input
      } else {
        get_validating_input(rest, out_ref)
      }
    [] -> error @"no script input found"
  }
}

pub fn get_reward_amt(
  withdraws: Dict<StakeCredential, Int>,
  stake_credential: StakeCredential,
) -> Int {
  when dict.get(withdraws, stake_credential) is {
    Some(reward) -> reward
    None -> error @"Nothing to be withdrawn"
  }
}
