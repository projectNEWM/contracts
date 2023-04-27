use aiken/dict
use aiken/interval.{Interval, IntervalBound, NegativeInfinity, PositiveInfinity}
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, TransactionId,
}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use fractional/sale_validators
use fractional/types.{OwnerInfo, SaleDatum, TokenInfo}

pub fn datum_a() -> SaleDatum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: Value =
    value.from_asset(#"acab", #"beef", 10)
  let max_bundle_size: Int =
    10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

pub fn datum_b() -> SaleDatum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"0102", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"acab", tkn: #"beef", amt: 10 }
  let cost: Value =
    value.from_asset(#"acab", #"beef", 10)
  let max_bundle_size: Int =
    10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

pub fn out_ref() -> OutputReference {
  OutputReference {
    transaction_id: TransactionId { hash: #"" },
    output_index: 0,
  }
}

pub fn tx_valid(
  script_addr: Address,
  other_addr: Address,
  addr: Address,
  out_addr: Address,
  signer,
) -> Transaction {
  let tx =
    Transaction {
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: TransactionId { hash: #"" },
            output_index: 0,
          },
          output: Output {
            address: script_addr,
            value: value.from_lovelace(10000),
            datum: InlineDatum(datum_a()),
            reference_script: None,
          },
        },
        Input {
          output_reference: OutputReference {
            transaction_id: TransactionId { hash: #"" },
            output_index: 1,
          },
          output: Output {
            address: other_addr,
            value: value.from_lovelace(10000),
            datum: InlineDatum(datum_b()),
            reference_script: None,
          },
        },
      ],
      reference_inputs: [],
      outputs: [
        Output {
          address: addr,
          value: value.from_lovelace(10000),
          datum: InlineDatum(datum_a()),
          reference_script: None,
        },
        Output {
          address: out_addr,
          value: value.from_lovelace(10000),
          datum: InlineDatum(datum_b()),
          reference_script: None,
        },
      ],
      fee: value.zero(),
      mint: value.zero(),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: Interval {
        lower_bound: IntervalBound {
          bound_type: NegativeInfinity,
          is_inclusive: True,
        },
        upper_bound: IntervalBound {
          bound_type: PositiveInfinity,
          is_inclusive: True,
        },
      },
      extra_signatories: [signer],
      redeemers: dict.new(),
      datums: dict.new(),
      id: TransactionId { hash: #"" },
    }
  tx
}

test good_update_tx() {
  let script_addr =
    credential.from_verification_key(#"0102")
      |> credential.with_delegation_key(#"")
  let addr =
    credential.from_verification_key(#"")
      |> credential.with_delegation_key(#"")
  sale_validators.update_validation(
    datum_a(),
    tx_valid(script_addr, addr, script_addr, addr, #""),
    out_ref(),
  ) == True
}

test bad_signer_update_tx() {
  let script_addr =
    credential.from_verification_key(#"0102")
      |> credential.with_delegation_key(#"")
  let addr =
    credential.from_verification_key(#"")
      |> credential.with_delegation_key(#"")
  sale_validators.update_validation(
    datum_a(),
    tx_valid(script_addr, addr, script_addr, addr, #"acab"),
    out_ref(),
  ) == False
}

// no way to test for errors yet
// test no_cont_update_tx() {
//   let script_addr = credential.from_verification_key(#"0102") |> credential.with_delegation_key(#"")
//   let addr = credential.from_verification_key(#"") |> credential.with_delegation_key(#"")
//   sale_validators.update_validation(
//     datum_a(),
//     tx_valid(script_addr, addr, addr, addr, #""),
//     out_ref(),
//   ) == False
// }

test two_inputs_update_tx() {
  let script_addr =
    credential.from_verification_key(#"0102")
      |> credential.with_delegation_key(#"")
  let addr =
    credential.from_verification_key(#"")
      |> credential.with_delegation_key(#"")
  sale_validators.update_validation(
    datum_a(),
    tx_valid(script_addr, script_addr, script_addr, addr, #""),
    out_ref(),
  ) == False
}

test two_ouputs_update_tx() {
  let script_addr =
    credential.from_verification_key(#"0102")
      |> credential.with_delegation_key(#"")
  let addr =
    credential.from_verification_key(#"")
      |> credential.with_delegation_key(#"")
  sale_validators.update_validation(
    datum_a(),
    tx_valid(script_addr, addr, script_addr, script_addr, #""),
    out_ref(),
  ) == False
}

test bad_cont_datum_update_tx() {
  let script_addr =
    credential.from_verification_key(#"0102")
      |> credential.with_delegation_key(#"")
  let addr =
    credential.from_verification_key(#"")
      |> credential.with_delegation_key(#"")
  sale_validators.update_validation(
    datum_a(),
    tx_valid(script_addr, addr, addr, script_addr, #""),
    out_ref(),
  ) == False
}
