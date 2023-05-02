use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, TransactionId,
}
use aiken/transaction/credential
use aiken/transaction/value
use fractional/helpers

// sample data for testing
type SampleData {
  a: Int,
  b: ByteArray,
}

test no_script_inputs() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let inputs =
    [
      Input {
        output_reference: OutputReference {
          transaction_id: TransactionId { hash: #"" },
          output_index: 0,
        },
        output: Output {
          address: addr,
          value: value.zero(),
          datum: InlineDatum(sample_datum),
          reference_script: None,
        },
      },
    ]
  helpers.count_script_inputs(
    inputs,
    credential.from_verification_key(#"") |> credential.with_delegation_key(#""),
    0,
  ) == 0
}

test single_script_input() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let inputs =
    [
      Input {
        output_reference: OutputReference {
          transaction_id: TransactionId { hash: #"" },
          output_index: 0,
        },
        output: Output {
          address: addr,
          value: value.zero(),
          datum: InlineDatum(sample_datum),
          reference_script: None,
        },
      },
    ]
  helpers.count_script_inputs(inputs, addr, 0) == 1
}

test no_script_output() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let outputs =
    [
      Output {
        address: addr,
        value: value.zero(),
        datum: InlineDatum(sample_datum),
        reference_script: None,
      },
    ]
  helpers.count_script_outputs(
    outputs,
    credential.from_verification_key(#"") |> credential.with_delegation_key(#""),
    0,
  ) == 0
}

test single_script_output() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let outputs =
    [
      Output {
        address: addr,
        value: value.zero(),
        datum: InlineDatum(sample_datum),
        reference_script: None,
      },
    ]
  helpers.count_script_outputs(outputs, addr, 0) == 1
}

test single_input_address() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let inputs =
    [
      Input {
        output_reference: OutputReference {
          transaction_id: TransactionId { hash: #"" },
          output_index: 0,
        },
        output: Output {
          address: addr,
          value: value.zero(),
          datum: InlineDatum(sample_datum),
          reference_script: None,
        },
      },
    ]
  let input =
    list.find(inputs, fn(input) { input.output.value == value.zero() })
  helpers.find_input_address(input) == addr
}

test single_cont_output() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let outputs =
    [
      Output {
        address: addr,
        value: value.zero(),
        datum: InlineDatum(sample_datum),
        reference_script: None,
      },
    ]
  helpers.find_cont_payout(addr, outputs) == True
}

test single_exact_payout() {
  let key_hash =
    #"010203040506"
  let sample_datum =
    SampleData { a: 1, b: #"01" }
  let addr =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let out_value =
    value.from_asset(#"", #"", 10)
  let outputs =
    [
      Output {
        address: addr,
        value: out_value,
        datum: InlineDatum(sample_datum),
        reference_script: None,
      },
    ]
  helpers.find_exact_payout(addr, out_value, outputs) == True
}

test addr_equality() {
  let key_hash =
    #"010203040506"
  let addr1 =
    credential.from_verification_key(key_hash)
      |> credential.with_delegation_key(#"")
  let addr2 =
    credential.from_verification_key(key_hash)
  addr1 != addr2
}

test script_addr() {
  helpers.create_script_address(#"acab", #"") != credential.from_verification_key(
    #"",
  )
}

test wallet_addr() {
  helpers.create_address(#"", #"") != credential.from_verification_key(#"acab")
}
