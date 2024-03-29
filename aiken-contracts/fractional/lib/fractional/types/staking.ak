use fractional/types/constants.{ValidatorHash}

pub type StakeData {
  stake_cred: ValidatorHash,
}

pub type WithdrawRedeemer {
  Withdraw
  Delegate(StakeData)
}
