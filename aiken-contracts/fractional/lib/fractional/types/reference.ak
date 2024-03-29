use aiken/transaction/value.{AssetName, PolicyId}
use assist/types/wallet.{Wallet}
use fractional/types/constants.{PublicKeyHash, ValidatorHash}

// staking info
pub type StakePool {
  pool_id: PoolId,
  reward_wallet: Wallet,
}

// keepers are like admins
pub type Keepers {
  pkhs: List<PublicKeyHash>,
  threshold: Int,
}

// contract validator hashes
pub type Contracts {
  cip68: ValidatorHash,
  sale: ValidatorHash,
  queue: ValidatorHash,
  band: ValidatorHash,
  stake: ValidatorHash,
}

pub type Fees {
  purchase_queue_bound: Int,
  refund_queue_bound: Int,
  start_sale_bound: Int,
}

pub type BatcherData {
  monster_pid: PolicyId,
  monster_tkns: List<AssetName>,
  batcher_pid: PolicyId,
  batcher_tkn: AssetName,
}

// data to be used inside other contracts
pub type ReferenceDatum {
  hot_key: PublicKeyHash,
  keepers: Keepers,
  staking: StakePool,
  contracts: Contracts,
  fee_info: Fees,
  pointer_pid: PolicyId,
  batcher_info: BatcherData,
}
