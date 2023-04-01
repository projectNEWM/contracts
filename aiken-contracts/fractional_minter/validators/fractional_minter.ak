use aiken/bytearray
use aiken/hash
use aiken/list
use aiken/transaction.{Input, Mint, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId}
use fractional_minter/helpers
use fractional_minter/types.{PublicKeyHash}

// (100) reference token prefix
const prefix_100 =
  #"2831303029"

// (333) fungible token prefix
const prefix_333 =
  #"2833333329"

// (444) semi-fungible token prefix
const prefix_444 =
  #"2834343429"

fn get_first_index(inputs: List<Input>) -> Int {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      error @"no index found"
    Some(input) ->
      input.output_reference.output_index
  }
}

fn get_first_ref(inputs: List<Input>) -> ByteArray {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      error @"no txid found"
    Some(input) ->
      input.output_reference.transaction_id.hash
  }
}

test length_of_prefix() {
  list.and(
    [
      bytearray.length(prefix_100) == 5,
      bytearray.length(prefix_333) == 5,
      bytearray.length(prefix_444) == 5,
    ],
  )
}

fn unique_token_name(id: ByteArray, idx: Int, prefix: ByteArray) -> ByteArray {
  // sha3_256 hash of the tx hash
  let tx_hash =
    hash.sha3_256(id)
  // prefix the hash with the index
  let indexed_hash =
    bytearray.push(tx_hash, idx)
  // concat the prefix
  let combined_hash =
    bytearray.concat(prefix, indexed_hash)
  // slice off the first 32
  bytearray.slice(combined_hash, 0, 32)
}

test simple_token() {
  unique_token_name(#"", 0, prefix_100) == #"283130302900a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d8"
}

test tx_hash_token() {
  let tkn =
    unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      1,
      prefix_333,
    )
  tkn == #"28333333290138c741df813afd1e2ba521d6b798dcabbc813ac7ba84467080b9"
}

test different_index() {
  let tkn1 =
    unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      prefix_333,
    )
  let tkn2 =
    unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      1,
      prefix_333,
    )
  tkn1 != tkn2
}

test different_prefix() {
  let tkn1 =
    unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      prefix_333,
    )
  let tkn2 =
    unique_token_name(
      #"1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b",
      0,
      prefix_444,
    )
  tkn1 != tkn2
}

fn correct_mint(
  mint: List<(PolicyId, AssetName, Int)>,
  pid: PolicyId,
  tkn: AssetName,
  amt: Int,
) -> Bool {
  when mint is {
    [(policy, token_name, quantity), ..rest] ->
      if list.and([policy == pid, token_name == tkn, quantity == amt]) == True {
        True
      } else {
        correct_mint(rest, pid, tkn, amt)
      }
    [] ->
      False
  }
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
      correct_mint(mint, #"acab", #"beef", 10) == True,
      correct_mint(mint, #"acab", #"bf", 20) == True,
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
      correct_mint(mint, #"acab", #"ac", 1) == False,
      correct_mint(mint, #"acab", #"bd", 100) == False,
    ],
  )
}

validator(newm_key_data: Data) {
  fn fractional_minter(_redeemer: Void, context: ScriptContext) -> Bool {
    expect newm_key: PublicKeyHash =
      newm_key_data
    let tx =
      context.transaction
    let first_index =
      get_first_index(tx.inputs)
    let first_tx_hash =
      get_first_ref(tx.inputs)
    // use the reference prefix
    let reference_tkn =
      unique_token_name(first_tx_hash, first_index, prefix_100)
    // use the sft prefix
    let fractions_tkn =
      unique_token_name(first_tx_hash, first_index, prefix_444)
    let mint =
      tx.mint
        |> value.without_lovelace()
        |> value.flatten()
    when context.purpose is {
      // Mint 1 ref token and 100M fractions
      Mint(own_currency_symbol) ->
        list.and(
          [
            helpers.must_be_signed_by(tx, newm_key)?,
            (first_index < 256)?,
            correct_mint(mint, own_currency_symbol, reference_tkn, 1)?,
            correct_mint(mint, own_currency_symbol, fractions_tkn, 100000000)?,
          ],
        )
      // Everything else fails
      _ ->
        False
    }
  }
}
