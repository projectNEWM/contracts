use aiken/cbor
use fractional/helpers

// An Int becomes a CBOR int
test cbor_int() {
  cbor.diagnostic(42) == @"42"
}

// A ByteArray becomes a CBOR bytestring
test cbor_string() {
  cbor.diagnostic(#"e6f85717b932788e3b1e57b11e9f8bf190d257c57d369979c18a02dd") == @"h'E6F85717B932788E3B1E57B11E9F8BF190D257C57D369979C18A02DD'"
}

test cbor_addr() {
  let addr1 =
    helpers.create_address(
      #"8a212f1b083647a955b020d4da04c735954c2b8d9d866d4be6c274f3",
      #"17b394e7cc269ba0f8a0ae3328fb08d43a5e46405448d56b4d963172",
    )
  let addr2 =
    helpers.create_script_address(
      #"8a212f1b083647a955b020d4da04c735954c2b8d9d866d4be6c274f3",
      #"17b394e7cc269ba0f8a0ae3328fb08d43a5e46405448d56b4d963172",
    )
  cbor.diagnostic(addr1) != cbor.diagnostic(addr2)
}

test cbor_addr2() {
  let addr2 =
    helpers.create_script_address(
      #"8a212f1b083647a955b020d4da04c735954c2b8d9d866d4be6c274f3",
      #"17b394e7cc269ba0f8a0ae3328fb08d43a5e46405448d56b4d963172",
    )
  let hex_encode =
    #"308a212f1b083647a955b020d4da04c735954c2b8d9d866d4be6c274f317b394e7cc269ba0f8a0ae3328fb08d43a5e46405448d56b4d963172"
  let shex =
    cbor.serialise(hex_encode)
  cbor.diagnostic(shex) != cbor.diagnostic(addr2)
}

test cbor_addr3() {
  cbor.serialise(
    #"308a212f1b083647a955b020d4da04c735954c2b8d9d866d4be6c274f317b394e7cc269ba0f8a0ae3328fb08d43a5e46405448d56b4d963172",
  ) != ""
}
