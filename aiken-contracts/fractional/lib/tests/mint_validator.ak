use aiken/bytearray
use aiken/list
use aiken/transaction/value
use fractional/helpers
use fractional/types

test check_if_it_has_a_prefix() {
  let x =
    #"28333333290138c741df813afd1e2ba521d6b798dcabbc813ac7ba84467080b9"
  let p =
    bytearray.take(x, 5)
  p == types.prefix_333
}

test length_of_prefix() {
  list.and(
    [
      bytearray.length(types.prefix_100) == 5,
      bytearray.length(types.prefix_333) == 5,
      bytearray.length(types.prefix_444) == 5,
    ],
  )
}

test simple_token() {
  helpers.unique_token_name(#"", 0, types.prefix_100) == #"283130302900a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a"
}

test tx_hash_token() {
  let tkn =
    helpers.unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      1,
      types.prefix_333,
    )
  tkn == #"28333333290138c741df813afd1e2ba521d6b798dcabbc813ac7ba84467080b9b6"
}

test different_index() {
  let tkn1 =
    helpers.unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      types.prefix_333,
    )
  let tkn2 =
    helpers.unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      1,
      types.prefix_333,
    )
  tkn1 != tkn2
}

test different_prefix() {
  let tkn1 =
    helpers.unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      types.prefix_333,
    )
  let tkn2 =
    helpers.unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      types.prefix_444,
    )
  tkn1 != tkn2
}

test good_mint() {
  let v =
    value.from_asset(#"acab", #"beef", 10)
  let w =
    value.from_asset(#"acab", #"bf", 20)
  let total =
    value.add(v, w)
  let mint =
    total
      |> value.without_lovelace()
      |> value.flatten()
  list.and(
    [
      helpers.correct_mint(mint, #"acab", #"beef", 10) == True,
      helpers.correct_mint(mint, #"acab", #"bf", 20) == True,
    ],
  )
}

test bad_mint() {
  let v =
    value.from_asset(#"acab", #"beef", 10)
  let w =
    value.from_asset(#"acab", #"bf", 20)
  let total =
    value.add(v, w)
  let mint =
    total
      |> value.without_lovelace()
      |> value.flatten()
  list.and(
    [
      helpers.correct_mint(mint, #"acab", #"ac", 1) == False,
      helpers.correct_mint(mint, #"acab", #"bd", 100) == False,
    ],
  )
}

test check_or() {
  list.and(
    [
      list.or([True, True]) == True,
      list.or([True, False]) == True,
      list.or([False, True]) == True,
      list.or([False, False]) == False,
    ],
  ) == True
}
