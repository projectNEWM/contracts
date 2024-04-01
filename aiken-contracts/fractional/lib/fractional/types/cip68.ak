use fractional/types/constants.{Lovelace}

pub type CIP68Datum {
  metadata: List<(Data, Data)>,
  version: Int,
}

// only can remove or update
pub type CIP68Redeemer {
  RemoveMeta
  UpdateMeta { lovelace: Lovelace }
}
