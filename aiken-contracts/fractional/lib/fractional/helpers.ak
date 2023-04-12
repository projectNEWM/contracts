use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash
use aiken/list
use aiken/transaction.{Input, Output, OutputReference, Transaction}
use aiken/transaction/credential.{Address, StakeCredential}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use fractional/data
use fractional/types.{PublicKeyHash}

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
    [] ->
      error @"no script input found"
  }
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
    [] ->
      counter
  }
}

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

pub fn create_address(pkh: PublicKeyHash, sc: PublicKeyHash) -> Address {
  // empty bytearrays means dont add the sc to the pkh
  if sc == #"" {
    credential.from_verification_key(pkh)
  } else {
    credential.from_verification_key(pkh)
      |> credential.with_delegation_key(sc)
  }
}

pub fn create_script_address(pkh: ByteArray, sc: ByteArray) -> Address {
  // empty bytearrays means dont add the sc to the pkh
  if sc == #"" {
    credential.from_script(pkh)
  } else {
    credential.from_script(pkh)
      |> credential.with_delegation_script(sc)
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

pub fn get_first_index(inputs: List<Input>) -> Int {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      error @"no index found"
    Some(input) ->
      input.output_reference.output_index
  }
}

pub fn get_first_ref(inputs: List<Input>) -> ByteArray {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      error @"no txid found"
    Some(input) ->
      input.output_reference.transaction_id.hash
  }
}

pub fn unique_token_name(
  id: ByteArray,
  idx: Int,
  prefix: ByteArray,
) -> ByteArray {
  // sha3_256 hash of the tx hash
  let tx_hash =
    hash.sha3_256(id)
  // prefix the hash with the index
  let indexed_hash =
    bytearray.push(tx_hash, idx)
  // concat the prefix
  let combined_hash =
    bytearray.concat(prefix, indexed_hash)
  // slice off the first 32
  bytearray.slice(combined_hash, 0, 32)
}

// using the validating input find the outbound datum
pub fn get_outbound_datum(tx: Transaction, validating_input: Input) -> Data {
  let script_addr =
    validating_input.output.address
  let outbound_output =
    find_script_output(tx.outputs, script_addr)
  data.find_outbound_datum(outbound_output)
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

pub fn find_input_address(possible_input: Option<Input>) -> Address {
  when possible_input is {
    Some(possible_input) ->
      possible_input.output.address
    None ->
      error @"can't find script address"
  }
}

pub fn correct_mint(
  mint: List<(PolicyId, AssetName, Int)>,
  pid: PolicyId,
  tkn: AssetName,
  amt: Int,
) -> Bool {
  when mint is {
    [(policy, token_name, quantity), ..rest] ->
      if list.and([policy == pid, token_name == tkn, quantity == amt]) == True {
        True
      } else {
        correct_mint(rest, pid, tkn, amt)
      }
    [] ->
      False
  }
}

pub fn correct_burn(
  mint: List<(PolicyId, AssetName, Int)>,
  pid: PolicyId,
  prefix: AssetName,
  amt: Int,
) -> Bool {
  when mint is {
    [(policy, token_name, quantity), ..rest] -> {
      // all prefixes have the same length
      let p =
        bytearray.take(token_name, 5)
      if list.and([policy == pid, prefix == p, quantity == amt]) == True {
        True
      } else {
        correct_burn(rest, pid, prefix, amt)
      }
    }
    [] ->
      False
  }
}

pub fn get_reward_amt(
  withdraws: Dict<StakeCredential, Int>,
  stake_credential: StakeCredential,
) -> Int {
  when dict.get(withdraws, stake_credential) is {
    Some(reward) ->
      reward
    None ->
      error @"Nothing to be withdrawn"
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
