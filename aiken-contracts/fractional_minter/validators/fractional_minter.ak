use aiken/bytearray
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{Input, Mint, ScriptContext, Transaction}
use aiken/transaction/credential.{VerificationKey}
use aiken/transaction/value.{AssetName, PolicyId}

type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

const prefix_100 =
  #"2831303029"

const prefix_333 =
  #"2833333329"

const newm_key =
  #"e6f85717b932788e3b1e57b11e9f8bf190d257c57d369979c18a02dd"

fn must_be_signed_by(transaction: Transaction, vk: PublicKeyHash) -> Bool {
  list.has(transaction.extra_signatories, vk)
}

fn get_first_index(inputs: List<Input>) -> Int {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      256
    Some(input) ->
      input.output_reference.output_index
  }
}

fn get_first_ref(inputs: List<Input>) -> ByteArray {
  let first_input =
    list.head(inputs)
  when first_input is {
    None ->
      #""
    Some(input) ->
      input.output_reference.transaction_id.hash
  }
}

test remove_first_byte() {
  bytearray.drop(#[1, 2, 3], n: 1) == #[2, 3]
}

test remove_n_bytes() {
  bytearray.drop(#[1, 2, 3, 4, 5, 6, 7, 8, 9], n: 4) == #[5, 6, 7, 8, 9]
}

test length_of_prefix() {
  bytearray.length(prefix_100) == 5 && bytearray.length(prefix_333) == 5
}

fn unique_token_name(id: ByteArray, idx: Int, prefix: ByteArray) -> ByteArray {
  let tx_hash =
    hash.sha3_256(id)

  let indexed =
    bytearray.push(tx_hash, idx)
  let combined =
    bytearray.concat(prefix, indexed)
  bytearray.slice(combined, 0, 32)
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

validator {
  fn fractional_minter(_redeemer: Void, context: ScriptContext) -> Bool {
    let tx =
      context.transaction
    let first_index =
      get_first_index(tx.inputs)
    let first_tx_hash =
      get_first_ref(tx.inputs)
    let reference_tkn =
      unique_token_name(first_tx_hash, first_index, prefix_100)
    let fractions_tkn =
      unique_token_name(first_tx_hash, first_index, prefix_333)
    let mint =
      tx.mint
        |> value.without_lovelace()
        |> value.flatten()
    when context.purpose is {
      Mint(own_currency_symbol) ->
        list.and(
          [
            must_be_signed_by(tx, newm_key)?,
            (first_index < 256)?,
            correct_mint(mint, own_currency_symbol, reference_tkn, 1)?,
            correct_mint(mint, own_currency_symbol, fractions_tkn, 100000000)?,
          ],
        )
      // Mint only
      _ ->
        False
    }
  }
}
