use aiken/list
use aiken/math/rational
use aiken/transaction/value
use fractional/types

test simple_ratio() {
  let x = rational.from_int(134)
  expect Some(p) = rational.new(3, 4)
  let o = rational.mul(x, p)
  rational.floor(o) == 100
}

test simple_ratio2() {
  let x = rational.from_int(156)
  let b = rational.from_int(100)
  expect Some(p) = rational.new(3, 4)
  expect Some(r) = rational.div(b, p)
  rational.floor(r) < rational.floor(x)
}

test simple_ratio3() {
  let x = rational.from_int(56)
  let b = rational.from_int(100)
  expect Some(p) = rational.new(3, 4)
  expect Some(r) = rational.div(b, p)
  rational.floor(r) > rational.floor(x)
}

test find_simple() {
  let x = rational.from_int(100)
  let b = rational.from_int(100)
  expect Some(p) = rational.new(3, 4)
  types.find_best(x, p, b) == 75
}

test find_simple2() {
  let x = rational.from_int(156)
  let b = rational.from_int(100)
  expect Some(p) = rational.new(3, 4)
  types.find_best(x, p, b) == 100
}

test find_simple3() {
  let x = rational.from_int(56)
  let b = rational.from_int(100)
  expect Some(p1) = rational.new(3, 4)
  expect Some(p2) = rational.new(4, 3)
  // types.find_best(x, p2, b) == types.find_best(b, p1, x)
  list.and([types.find_best(x, p2, b) == 74, types.find_best(b, p1, x) == 56])
}

test find_simple4() {
  let x = rational.from_int(2000000)
  let b = rational.from_int(2000000)
  expect Some(p) = rational.new(1, 1)
  types.find_best(x, p, b) == 2000000
}

// test find_moderate() {
//   let x = rational.from_int(134)
//   let b = rational.from_int(12345678)
//   expect Some(p) = rational.new(3, 4)
//   types.find_best(rational.zero(), p, b) == 100
// }

test value_test1() {
  // how much I have
  let this_amt_ratio = rational.from_int(1234567)
  // how much they have
  let that_amt_ratio = rational.from_int(7654321)
  // my price ratio
  expect Some(this_price_ratio) = rational.new(4, 3)
  // their price ratio
  expect Some(that_price_ratio) = rational.new(3, 4)
  // how many this tokens they get
  let they_get_amt =
    types.find_best(that_amt_ratio, this_price_ratio, this_amt_ratio)
  let they_get_value = value.from_asset(#"acab", #"beef", they_get_amt)
  // this value changes
  let my_get_amt =
    types.find_best(this_amt_ratio, that_price_ratio, that_amt_ratio)
  let my_get_value = value.from_asset(#"fade", #"cafe", my_get_amt)

  let that_value =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"fade", #"cafe", 7654321))

  let that_cont_value =
    value.merge(that_value, they_get_value)
      |> value.merge(value.negate(my_get_value))
  let that_result =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"fade", #"cafe", 7654321 - my_get_amt))
      |> value.merge(value.from_asset(#"acab", #"beef", 0 + they_get_amt))

  that_cont_value == that_result
}
