use aiken/transaction/value
use fractional/helpers

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
  helpers.does_value_exist_in_value(total, flat) == True
}

test check_no_value_in_value() {
  let total =
    value.from_lovelace(100)
      |> value.add(value.from_asset(#"acab", #"beef", 10))
      |> value.add(value.from_asset(#"face", #"cafe", 1))
      |> value.add(value.from_asset(#"deaf", #"fade", 100))
  let flat =
    value.from_asset(#"face", #"dace", 1) |> value.flatten()
  helpers.does_value_exist_in_value(total, flat) == False
}
