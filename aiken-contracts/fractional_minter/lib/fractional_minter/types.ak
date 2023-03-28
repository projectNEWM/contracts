use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{VerificationKey}

pub type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type CIP68Datum {
  metadata: Dict<Data, Data>,
  version: Int,
}

pub type LovelaceData {
  increase: Int,
}

pub type Remove =
  Void

pub type CIP68Redeemer {
  Remove
  Update { lovelace: LovelaceData }
}
