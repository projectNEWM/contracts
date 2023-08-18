use aiken/bytearray

test sub_string_in_string1() {
  let t = #"acabbeeffacecafe01"
  let x = #"acabbeef"

  let lx = bytearray.length(x) - 1
  bytearray.slice(t, start: 0, end: lx) == x
}
