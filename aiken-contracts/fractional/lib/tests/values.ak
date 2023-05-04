use aiken/transaction/value
use fractional/helpers
use fractional/types.{TokenInfo}

test dont_value_multiply() {
  let val =
    value.from_asset(#"", #"", 10)
  helpers.multiply_value(value.zero(), val, 0) == value.zero()
}

test simple_value_multiply() {
  let val =
    value.from_asset(#"", #"", 10)
  let total =
    value.from_asset(#"", #"", 100)
  helpers.multiply_value(value.zero(), val, 10) == total
}

test chain_value_add() {
  value.from_lovelace(100) == value.from_lovelace(100)
}

test can_if_be_value() {
  let v =
    if 10 / 10 <= 1 {
      0
    } else {
      1
    }
  v == 0
}

test check_value_in_value() {
  let total =
    value.from_lovelace(100)
      |> value.add(value.from_asset(#"acab", #"beef", 10))
      |> value.add(value.from_asset(#"face", #"cafe", 1))
      |> value.add(value.from_asset(#"deaf", #"fade", 100))
  let flat =
    value.from_asset(#"face", #"cafe", 1) |> value.flatten()
  helpers.does_value_exist_in_value(flat, total) == True
}

test check_no_value_in_value() {
  let total =
    value.from_lovelace(100)
      |> value.add(value.from_asset(#"acab", #"beef", 10))
      |> value.add(value.from_asset(#"face", #"cafe", 1))
      |> value.add(value.from_asset(#"deaf", #"fade", 100))
  let flat =
    value.from_asset(#"face", #"dace", 1) |> value.flatten()
  helpers.does_value_exist_in_value(flat, total) == False
}

test check_value_hash() {
  let total =
    value.from_lovelace(100)
      |> value.add(value.from_asset(#"acab", #"beef", 10))
      |> value.add(value.from_asset(#"face", #"cafe", 1))
      |> value.add(value.from_asset(#"deaf", #"fade", 100))
      |> value.flatten()
  helpers.compute_value_hash(total, #"") == #"bb9657754b6bd0dba61d7ee4ae4232182bb192152c115d151d27d5fc48b94965"
}

test check_token_info_hash() {
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 0 }
  helpers.compute_token_info_hash(bundle) == #"3ea6f93cd00a9dc6d9c22dea97662fd23156200e4da06f02f0b59f2b640675be"
}

test check_merkle_like_hash() {
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 0 }
  let cost_value =
    value.from_lovelace(100)
      |> value.add(value.from_asset(#"acab", #"beef", 10))
      |> value.add(value.from_asset(#"face", #"cafe", 1))
      |> value.add(value.from_asset(#"deaf", #"fade", 100))
      |> value.flatten()
  helpers.compute_sale_hash(bundle, cost_value) == #"4a7253484cbab9cae08004401e0f3dc3402ff27150a46937161f088157c831da"
}
