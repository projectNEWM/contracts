use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/transaction.{DatumHash, InlineDatum, Output}
use aiken/transaction/credential.{Address}
use assist/find

/// Find the first occurrance of an output with a specific address and return
/// the data on the UTxO. This works for both inline datums and datum hashes.
/// If nothing is found then fail else return data.
///
/// ```aiken
/// util.output_datum(tx.outputs, this_addr, tx.datums)
/// ```
pub fn output_datum(
  outputs: List<Output>,
  address: Address,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
) -> Data {
  let outbound_output: Output = find.output_by_addr(outputs, address)
  when outbound_output.datum is {
    // find the inline
    InlineDatum(outbound_datum) -> outbound_datum
    // find the embedded
    DatumHash(outbound_datum_hash) ->
      when dict.get(datums, outbound_datum_hash) is {
        Some(outbound_datum) -> outbound_datum
        _ -> fail @"Datum Hash Not Found"
      }
    // fail on no Datum
    _ -> fail @"No Datum Found"
  }
}
