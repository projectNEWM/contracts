use aiken/dict.{Dict}
use fractional/types/constants.{Lovelace}

pub type CIP68Datum {
  metadata: Dict<Data, Data>,
  version: Int,
}

// only can remove or update
pub type CIP68Redeemer {
  RemoveMeta
  UpdateMeta { lovelace: Lovelace }
}
