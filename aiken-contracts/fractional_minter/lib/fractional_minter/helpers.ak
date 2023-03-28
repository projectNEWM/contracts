use aiken/list
use aiken/transaction.{Input, Output, OutputReference, Transaction}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use fractional_minter/types.{PublicKeyHash}

pub fn must_be_signed_by(transaction: Transaction, vk: PublicKeyHash) -> Bool {
  list.has(transaction.extra_signatories, vk)
}

pub fn get_input_by_ref(inputs: List<Input>, out_ref: OutputReference) -> Input {
  when inputs is {
    [input, ..rest] ->
      if input.output_reference == out_ref {
        input
      } else {
        get_input_by_ref(rest, out_ref)
      }
    [] ->
      error @"no input found"
  }
}

pub fn count_script_inputs(
  inputs: List<Input>,
  script_addr: Address,
  counter: Int,
) -> Int {
  when inputs is {
    [input, ..rest] ->
      if input.output.address == script_addr {
        count_script_inputs(rest, script_addr, counter + 1)
      } else {
        count_script_inputs(rest, script_addr, counter)
      }
    [] ->
      counter
  }
}

// need function that counts script outputs
pub fn count_script_outputs(
  outputs: List<Output>,
  script_addr: Address,
  counter: Int,
) -> Int {
  when outputs is {
    [output, ..rest] ->
      if output.address == script_addr {
        count_script_outputs(rest, script_addr, counter + 1)
      } else {
        count_script_outputs(rest, script_addr, counter)
      }
    [] ->
      counter
  }
}

pub fn find_exact_payout(
  pay_address: Address,
  pay_value: Value,
  outputs: List<Output>,
) -> Bool {
  when outputs is {
    [output, ..rest] ->
      if output.address == pay_address && output.value == pay_value {
        True
      } else {
        find_exact_payout(pay_address, pay_value, rest)
      }
    [] ->
      False
  }
}
