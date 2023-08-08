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
