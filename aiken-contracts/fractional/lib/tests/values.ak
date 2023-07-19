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

// but dont do things on the fee unless it must be split up
test merge_or_add3() {
  let validating_value = value.from_lovelace(100)
  let cost_value = value.from_asset(#"acab", #"beef", 54)
  let result =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"cafe", #"fade", 10 * 11)

  let return_sale_value =
    value.merge(validating_value, cost_value)
      |> value.add(#"cafe", #"fade", 10 * 11)
  return_sale_value == result
}

// but dont do things on the fee unless it must be split up
test merge_or_add4() {
  let fee = value.from_lovelace(54)
  let cost_value = value.from_asset(#"acab", #"beef", 54)
  let bundle_value = value.from_asset(#"fade", #"cafe", 40)
  let validating_value =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"aabb", #"ccdd", 20)
      |> value.add(#"cafe", #"fade", 1)

  let result =
    value.from_lovelace(46)
      |> value.add(#"cafe", #"fade", 1)
      |> value.merge(bundle_value)

  let return_queue_value =
    value.merge(validating_value, value.negate(cost_value))
      |> value.merge(value.negate(fee))
      |> value.add(#"fade", #"cafe", 40)
      |> value.add(#"aabb", #"ccdd", -20)
  return_queue_value == result
}
