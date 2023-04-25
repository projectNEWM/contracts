use aiken/cbor

// An Int becomes a CBOR int
test cbor_int() {
  cbor.diagnostic(42) == @"42"
}

// A ByteArray becomes a CBOR bytestring
test cbor_string() {
  cbor.diagnostic(#"e6f85717b932788e3b1e57b11e9f8bf190d257c57d369979c18a02dd") == @"h'E6F85717B932788E3B1E57B11E9F8BF190D257C57D369979C18A02DD'"
}
