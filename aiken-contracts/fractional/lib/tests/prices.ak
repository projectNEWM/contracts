use aiken/list
use aiken/math/rational
use fractional/types

test simple_price1() {
  expect Some(this_total) = rational.new(12345678, 1)
  expect Some(price) = rational.new(1, 1)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(12345678, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 12345678,
      types.find_best(that_total, reciprocal_price, this_total) == 12345678,
    ],
  )
}

test simple_price2() {
  expect Some(this_total) = rational.new(12345678, 1)
  expect Some(price) = rational.new(135, 751)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(12345, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 12345,
      types.find_best(that_total, reciprocal_price, this_total) == 68674,
    ],
  )
}

test simple_price3() {
  expect Some(this_total) = rational.new(150000000000, 1)
  expect Some(price) = rational.new(127, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(123456789, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 123456789,
      types.find_best(that_total, reciprocal_price, this_total) == 9721007007,
    ],
  )
}

test simple_price4() {
  expect Some(this_total) = rational.new(150000000000, 1)
  expect Some(price) = rational.new(127, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(3234567890, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 1905000000,
      types.find_best(that_total, reciprocal_price, this_total) == 150000000000,
    ],
  )
}

test simple_price5() {
  // this is why thresholds need to exist
  expect Some(this_total) = rational.new(1, 1)
  expect Some(price) = rational.new(127, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(1, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 0,
      types.find_best(that_total, reciprocal_price, this_total) == 1,
    ],
  )
}

test simple_price6() {
  // this is why thresholds need to exist
  expect Some(this_total) = rational.new(1, 1)
  expect Some(price) = rational.new(9999, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(1, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 0,
      types.find_best(that_total, reciprocal_price, this_total) == 1,
    ],
  )
}

test simple_price7() {
  // this is why thresholds need to exist
  expect Some(this_total) = rational.new(123456789, 1)
  expect Some(price) = rational.new(9999, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(987654321, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 123444443,
      types.find_best(that_total, reciprocal_price, this_total) == 123456789,
    ],
  )
}

test simple_price8() {
  // this is why thresholds need to exist
  expect Some(this_total) = rational.new(123, 1)
  expect Some(price) = rational.new(127, 10000)
  expect Some(reciprocal_price) = rational.reciprocal(price)
  expect Some(that_total) = rational.new(321, 1)
  list.and(
    [
      types.find_best(this_total, price, that_total) == 1,
      types.find_best(that_total, reciprocal_price, this_total) == 123,
    ],
  )
}
