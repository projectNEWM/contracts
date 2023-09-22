use aiken/math
use fractional/types

test sqrt1() {
  let number = math.pow(10, 7) - 1
  types.sqrt(number) > 0
}

test sqrt2() {
  let number = math.pow(10, 14) - 1
  types.sqrt(number) > 0
}

test sqrt3() {
  let number = math.pow(10, 21) - 1
  types.sqrt(number) > 0
}

test sqrt4() {
  let number = math.pow(10, 28) - 1
  types.sqrt(number) > 0
}

test sqrt5() {
  let number = math.pow(10, 35) - 1
  types.sqrt(number) > 0
}

test sqrt6() {
  let number = math.pow(10, 42) - 1
  types.sqrt(number) > 0
}

test sqrt_square1() {
  let number = math.pow(123, 2)
  types.sqrt(number) == 123
}

test sqrt_square2() {
  let number = math.pow(123456, 2)
  types.sqrt(number) == 123456
}

test sqrt_square3() {
  let number = math.pow(1234567890, 2)
  types.sqrt(number) == 1234567890
}
