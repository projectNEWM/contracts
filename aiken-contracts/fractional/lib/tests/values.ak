use aiken/transaction/value

// when its just the fee use this merge
test merge_or_add1() {
  let validating_value = value.from_lovelace(100)
  let fee = value.from_lovelace(54)
  let result = value.from_lovelace(46)
  let outbound_value = value.merge(validating_value, value.negate(fee))
  outbound_value == result
}

// but dont do things on the fee unless it must be split up
test merge_or_add2() {
  let validating_value = value.from_lovelace(100)
  let fee = value.from_lovelace(54)
  let result = value.from_lovelace(46)

  let outbound_value =
    value.add(
      validating_value,
      value.ada_policy_id,
      value.ada_asset_name,
      -value.lovelace_of(fee),
    )
  outbound_value == result
}
