use aiken/transaction.{Input, Output}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, Value}

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

pub fn find_input_address(possible_input: Option<Input>) -> Address {
  when possible_input is {
    Some(possible_input) ->
      possible_input.output.address
    None ->
      error @"can't find script address"
  }
}

pub fn find_script_input(inputs: List<Input>, script_addr: Address) -> Input {
  when inputs is {
    [input, ..rest] ->
      if input.output.address == script_addr {
        input
      } else {
        find_script_input(rest, script_addr)
      }
    [] ->
      error @"no script input found"
  }
}

pub fn find_script_output(outputs: List<Output>, script_addr: Address) -> Output {
  when outputs is {
    [output, ..rest] ->
      if output.address == script_addr {
        output
      } else {
        find_script_output(rest, script_addr)
      }
    [] ->
      error @"no script output found"
  }
}

pub fn find_cont_payout(pay_address: Address, outputs: List<Output>) -> Bool {
  when outputs is {
    [output, ..rest] ->
      if output.address == pay_address {
        True
      } else {
        find_cont_payout(pay_address, rest)
      }
    [] ->
      False
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

pub fn find_token_payout(
  pay_address: Address,
  pay_value: Value,
  outputs: List<Output>,
) -> Bool {
  let flattened_value =
    value.flatten(pay_value)
  when outputs is {
    [output, ..rest] ->
      if
      output.address == pay_address && check_all_assets(
        output.value,
        flattened_value,
      ) == True{
      
        True
      } else {
        find_token_payout(pay_address, pay_value, rest)
      }
    [] ->
      False
  }
}

fn check_all_assets(
  out_value: Value,
  flattened_value: List<(PolicyId, AssetName, Int)>,
) -> Bool {
  when flattened_value is {
    [(policy, token_name, quantity), ..rest] ->
      if value.quantity_of(out_value, policy, token_name) >= quantity {
        check_all_assets(out_value, rest)
      } else {
        False
      }
    [] ->
      True
  }
}

pub fn create_address(pkh: ByteArray, sc: ByteArray) -> Address {
  // empty bytearrays means dont add the sc to the pkh
  if sc == #"" {
    credential.from_verification_key(pkh)
  } else {
    credential.from_verification_key(pkh)
      |> credential.with_delegation_key(sc)
  }
}
