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
