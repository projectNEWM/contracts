use aiken/list
use aiken/math/rational
use aiken/transaction/value
use fractional/types.{Ratio}

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

// test find_simple5() {
//   let x = rational.from_int(2000000)
//   let b = rational.from_int(2000000)
//   expect Some(p1) = rational.new(3, 4)
//   expect Some(p2) = rational.new(4, 3)
//   types.find_best(x, p1, b) == types.find_best(b, p2, x)
// }

// test find_moderate() {
//   let x = rational.from_int(134)
//   let b = rational.from_int(12345678)
//   expect Some(p) = rational.new(3, 4)
//   types.find_best(rational.zero(), p, b) == 100
// }

test value_test1() {
  let this_amt = 2000000
  let that_amt = 2000000
  // how much I have
  let this_amt_ratio = rational.from_int(this_amt)
  // how much they have
  let that_amt_ratio = rational.from_int(that_amt)

  // my price ratio
  // expect Some(this_price_ratio) = rational.new(3, 4)
  // // their price ratio
  // expect Some(that_price_ratio) = rational.new(4, 3)
  // // this has units of this per that
  // expect Some(reciprocal_that_price_ratio) =
  //   rational.reciprocal(that_price_ratio)
  // expect Some(one_half) = rational.reciprocal(rational.from_int(2))
  // let average_price =
  //   rational.mul(
  //     one_half,
  //     rational.add(this_price_ratio, reciprocal_that_price_ratio),
  //   )
  // expect Some(reciprocal_average_price) = rational.reciprocal(that_price_ratio)
  // get the price ratio
  let this_price = Ratio { numerator: 67, denominator: 2500 }
  let that_price = Ratio { numerator: 35331, denominator: 1000 }
  let average_price =
    types.ratio_average(this_price, that_price)
      |> types.ratio_to_rational()
  expect Some(reciprocal_average_price) = rational.reciprocal(average_price)

  // that value changes
  let that_get_amt =
    types.find_best(that_amt_ratio, average_price, this_amt_ratio)
  let that_get_value = value.from_asset(#"acab", #"beef", that_get_amt)

  // this value changes
  let this_get_amt =
    types.find_best(this_amt_ratio, reciprocal_average_price, that_amt_ratio)
  let this_get_value = value.from_asset(#"fade", #"cafe", this_get_amt)

  // let my_get_value = value.from_asset(#"fade", #"cafe", my_get_amt)
  let that_value =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"fade", #"cafe", that_amt))

  let this_value =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"acab", #"beef", this_amt))

  let that_cont_value =
    value.merge(that_value, that_get_value)
      |> value.merge(value.negate(this_get_value))
  let that_result =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"fade", #"cafe", that_amt - this_get_amt))
      |> value.merge(value.from_asset(#"acab", #"beef", 0 + that_get_amt))

  let this_cont_value =
    value.merge(this_value, this_get_value)
      |> value.merge(value.negate(that_get_value))
  let this_result =
    value.from_lovelace(987654321)
      |> value.merge(value.from_asset(#"acab", #"beef", this_amt - that_get_amt))
      |> value.merge(value.from_asset(#"fade", #"cafe", 0 + this_get_amt))
  that_cont_value == that_result && this_cont_value == this_result
  // that_cont_value != that_result
  // this_get_amt == that_get_amt
}

test simple_sqrt() {
  types.sqrt(25) == 5
}

test moderate_sqrt() {
  types.sqrt(9871) == 99
}

test large_sqrt() {
  types.sqrt(987654321) == 31426
}

test huge_sqrt() {
  types.sqrt(9876543211234567890) == 3142696805
}

test simple_average_sqrt() {
  let this_price = Ratio { numerator: 67, denominator: 2500 }
  let that_price = Ratio { numerator: 35331, denominator: 1000 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  expect Some(right) =
    types.ratio_average(that_price, this_price)
      |> types.ratio_to_rational()
      |> rational.reciprocal()
  left == right
}

test self_average_sqrt() {
  let this_price = Ratio { numerator: 67, denominator: 2500 }
  let that_price = Ratio { numerator: 35331, denominator: 1000 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  left == left
}

test small_average_sqrt() {
  let this_price = Ratio { numerator: 17, denominator: 20 }
  let that_price = Ratio { numerator: 21, denominator: 18 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  left == left
}

test medium_average_sqrt() {
  let this_price = Ratio { numerator: 17321, denominator: 20123 }
  let that_price = Ratio { numerator: 21321, denominator: 18123 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  left == left
}

test big_self_average_sqrt() {
  let this_price = Ratio { numerator: 827851, denominator: 30889997 }
  let that_price = Ratio { numerator: 436196523, denominator: 12345997 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  left == left
}

test huge_self_average_sqrt() {
  let this_price =
    Ratio { numerator: 890876827851, denominator: 34532130889997 }
  let that_price =
    Ratio { numerator: 456432436196523, denominator: 19812312345997 }
  let left =
    types.ratio_average(this_price, that_price) |> types.ratio_to_rational()
  left == left
}
