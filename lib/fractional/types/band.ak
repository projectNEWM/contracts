use aiken/bytearray
use aiken/transaction/value.{AssetName}
use assist/types/token.{Tokens}
use assist/types/wallet.{Wallet}

// We need some data types for the new monster batcher nft pass
pub type BandLockUpDatum {
  owner: Wallet,
}

pub type BandLockUpRedeemer {
  RemoveBand
  AddToBand { members: Tokens }
  MintBand
  BurnBand
}

/// Token names are from the value or total value. Monster token names are the
/// official prefixes for the monster nfts.
///
pub fn has_correct_tokens(
  tkns_from_value: List<AssetName>,
  tkn_prefixes: List<ByteArray>,
) -> Bool {
  when tkn_prefixes is {
    // found all of them in the value
    [] -> True
    // go through all the prefixes from the set
    [prefix, ..prefixes] ->
      if do_has_correct_tokens(tkns_from_value, prefix) {
        // found something so go to the next one
        has_correct_tokens(tkns_from_value, prefixes)
      } else {
        // missing something from the set
        False
      }
  }
}

fn do_has_correct_tokens(
  tkns_from_value: List<AssetName>,
  prefix: ByteArray,
) -> Bool {
  when tkns_from_value is {
    // something wasn't found
    [] -> False
    [tkn, ..tkns] ->
      if prefix_in_token_name(prefix, tkn) {
        // found it
        True
      } else {
        // keep searching
        do_has_correct_tokens(tkns, prefix)
      }
  }
}

/// Check if some known prefix exists within some token name.
fn prefix_in_token_name(prefix: ByteArray, token_name: AssetName) -> Bool {
  // the length of the prefix counting from 0
  let length_of_prefix = bytearray.length(prefix) - 1
  // excluding the number does at least the prefix match
  bytearray.slice(token_name, start: 0, end: length_of_prefix) == prefix
}
