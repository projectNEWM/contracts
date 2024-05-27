use fractional/types/constants.{Lovelace}

// only can remove or update
pub type CIP68Redeemer {
  RemoveMeta
  UpdateMeta { lovelace: Lovelace }
}
