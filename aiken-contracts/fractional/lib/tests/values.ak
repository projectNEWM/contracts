use aiken/bytearray
use aiken/dict.{Dict}
use aiken/list
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

test asc_list_value1() {
  let cost_value = value.from_asset(#"acab", #"beef", 54) |> value.to_dict()
  let tokens = dict.from_ascending_list([(#"beef", 54)], bytearray.compare)
  let result = dict.from_ascending_list([(#"acab", tokens)], bytearray.compare)
  cost_value == result
}

test asc_list_value2() {
  let cost_value =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"cafe", #"fade", 101)
      |> value.to_dict()
  let tokens0 = dict.from_ascending_list([(#"", 100)], bytearray.compare)
  let tokens1 = dict.from_ascending_list([(#"beef", 54)], bytearray.compare)
  let tokens2 = dict.from_ascending_list([(#"fade", 101)], bytearray.compare)
  let result =
    dict.from_ascending_list(
      [(#"", tokens0), (#"acab", tokens1), (#"cafe", tokens2)],
      bytearray.compare,
    )
  cost_value == result
}

type AllTokenInfo {
  pid: ByteArray,
  tkns: List<ByteArray>,
  amts: List<Int>,
}

type Tokens {
  tkns: List<AllTokenInfo>,
}

/// Tokens will be assumed to be in order and are unique. Well prepped off chain.
fn convert_tokens(tkns: Tokens) -> Dict<ByteArray, Dict<ByteArray, Int>> {
  convert_token_info_to_dict(tkns.tkns, [])
}

fn convert_token_info_to_dict(
  tkns: List<AllTokenInfo>,
  builder: List<(ByteArray, Dict<ByteArray, Int>)>,
) -> Dict<ByteArray, Dict<ByteArray, Int>> {
  when tkns is {
    // processed all of them build final dict
    [] -> dict.from_ascending_list(builder, bytearray.compare)
    // go through all the tokens from the set
    [alltkn, ..rest] -> {
      // there may be many tkns to a pid
      let token_tuple: List<(ByteArray, Int)> =
        list.map2(alltkn.tkns, alltkn.amts, fn(a, b) { (a, b) })
      // build out the token dict
      let tokens: Dict<ByteArray, Int> =
        dict.from_ascending_list(token_tuple, bytearray.compare)
      // continue to the next one
      convert_token_info_to_dict(
        rest,
        list.concat(builder, [(alltkn.pid, tokens)]),
      )
    }
  }
}

test token_to_dict1() {
  let lovelace: AllTokenInfo =
    AllTokenInfo { pid: #"", tkns: [#""], amts: [100] }
  let tokens: Tokens = Tokens { tkns: [lovelace] }
  let result = convert_tokens(tokens)
  let cost_value =
    value.from_lovelace(100)
      |> value.to_dict()
  result == cost_value
}

test token_to_dict2() {
  let lovelace: AllTokenInfo =
    AllTokenInfo { pid: #"", tkns: [#""], amts: [100] }
  let tkn1: AllTokenInfo =
    AllTokenInfo { pid: #"acab", tkns: [#"beef", #"cafe"], amts: [54, 101] }
  let tokens: Tokens = Tokens { tkns: [lovelace, tkn1] }
  let result = convert_tokens(tokens)
  let cost_value =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"acab", #"cafe", 101)
      |> value.to_dict()
  result == cost_value
}

test token_to_dict3() {
  let lovelace: AllTokenInfo =
    AllTokenInfo { pid: #"", tkns: [#""], amts: [100] }
  let tkn1: AllTokenInfo =
    AllTokenInfo { pid: #"acab", tkns: [#"beef"], amts: [54] }
  let tkn2: AllTokenInfo =
    AllTokenInfo { pid: #"cafe", tkns: [#"fade"], amts: [101] }
  let tokens: Tokens = Tokens { tkns: [lovelace, tkn1, tkn2] }
  let result = convert_tokens(tokens)
  let cost_value =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"cafe", #"fade", 101)
      |> value.to_dict()
  result == cost_value
}

test token_to_dict4() {
  let lovelace: AllTokenInfo =
    AllTokenInfo { pid: #"", tkns: [#""], amts: [100] }
  let tkn1: AllTokenInfo =
    AllTokenInfo { pid: #"acab", tkns: [#"beef"], amts: [54] }
  let tokens: Tokens = Tokens { tkns: [lovelace, tkn1] }
  let result = convert_tokens(tokens)
  let cost_value =
    value.from_lovelace(100)
      |> value.add(#"acab", #"beef", 54)
      |> value.add(#"cafe", #"fade", 0)
      |> value.to_dict()
  result == cost_value
}
