use aiken/bytearray
use aiken/list
use aiken/transaction/value.{AssetName, PolicyId}

pub fn correct_mint(
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
    // something wasn't found
    [] ->
      False
  }
}

pub fn correct_burn(
  mint: List<(PolicyId, AssetName, Int)>,
  pid: PolicyId,
  prefix: AssetName,
  amt: Int,
) -> Bool {
  when mint is {
    [(policy, token_name, quantity), ..rest] -> {
      // all prefixes have the same length
      let p =
        bytearray.take(token_name, 5)
      if list.and([policy == pid, prefix == p, quantity == amt]) == True {
        True
      } else {
        correct_burn(rest, pid, prefix, amt)
      }
    }
    // something wasn't found
    [] ->
      False
  }
}
