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
