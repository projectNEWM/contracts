use aiken/bytearray

test sub_string_in_string1() {
  let t = #"acabbeeffacecafe01"
  let x = #"acabbeef"

  let lx = bytearray.length(x) - 1
  bytearray.slice(t, start: 0, end: lx) == x
}
// test sub_string_in_string2() {
//   let t = #"4e45574d6f6e737465725069616e69737436"
//   let xs = [
//     #"4e45574d6f6e737465725069616e697374",
//     #"4e45574d6f6e73746572526f636b",
//     #"4e45574d6f6e73746572444a",
//     #"4e45574d6f6e73746572486970486f70",
//     #"4e45574d6f6e737465724f70657261",
//     #"4e45574d6f6e73746572526567676165".
//     #"4e45574d6f6e737465724865617679",
//     #"4e45574d6f6e737465724a617a7a",
//     #"4e45574d6f6e7374657248656c6c6f",
//     #"4e45574d6f6e73746572466c616d656e636f",
//     #"4e45574d6f6e7374657252616e6368657261",
//     #"4e45574d6f6e73746572446973636f",
//   ]

//   let lx = bytearray.length(x) - 1
//   bytearray.slice(t, start: 0, end: lx) == x
// }
