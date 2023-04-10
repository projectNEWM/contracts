use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{PoolId, Script, VerificationKey}

// (100) reference token prefix
pub const prefix_100 =
  #"2831303029"

// (333) fungible token prefix
pub const prefix_333 =
  #"2833333329"

// (444) semi-fungible token prefix
pub const prefix_444 =
  #"2834343429"

pub type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type ValidatorHash =
  Hash<Blake2b_224, Script>

pub type CIP68Datum {
  metadata: Dict<Data, Data>,
  version: Int,
}

pub type LovelaceData {
  increase: Int,
}

pub type RemoveMeta =
  Void

pub type RemoveSale =
  Void

pub type CIP68Redeemer {
  RemoveMeta
  UpdateMeta { lovelace: LovelaceData }
}

pub type BuyAmount {
  amt: Int,
}

pub type UpdateSale =
  Void

pub type SaleRedeemer {
  Purchase { buy_amt: BuyAmount }
  UpdateSale
  RemoveSale
}

const max_bundle_size =
  10

pub fn valid_bundle_size(buy_amt: BuyAmount) -> Bool {
  buy_amt.amt > 0 && buy_amt.amt <= max_bundle_size
}

pub type OwnerInfo {
  pkh: PublicKeyHash,
  sc: PublicKeyHash,
}

pub type TokenInfo {
  pid: ByteArray,
  tkn: ByteArray,
  amt: Int,
}

pub type SaleDatum {
  owner: OwnerInfo,
  bundle: TokenInfo,
  cost: TokenInfo,
}

pub fn purchase_datum_check(
  inbound_datum: SaleDatum,
  outbound_datum: SaleDatum,
) -> Bool {
  inbound_datum == outbound_datum
}

// keepers are like admins
pub type KeeperData {
  pkhs: List<PublicKeyHash>,
  threshold: Int,
}

// staking info
pub type StakePoolData {
  pool_id: PoolId,
  reward_pkh: PublicKeyHash,
  reward_sc: PublicKeyHash,
}

// contract validator hashes
pub type ContractHashData {
  cip68: ValidatorHash,
  sale: ValidatorHash,
  stake: ValidatorHash,
}

pub type ReferenceDatum {
  hot_key: PublicKeyHash,
  keepers: KeeperData,
  staking: StakePoolData,
  contracts: ContractHashData,
}

pub type UpdateHotKey =
  Void

pub type UpdateKeepers =
  Void

pub type UpdateStaking =
  Void

pub type UpdateContracts =
  Void

pub type ReferenceRedeemer {
  UpdateHotKey
  UpdateKeepers
  UpdateStaking
  UpdateContracts
}

pub type StakeData {
  stake_cred: Hash<Blake2b_224, Script>,
}

pub type WithdrawRedeemer {
  Withdraw
  Delegate(StakeData)
}

pub type MintRedeemer {
  MintTokens
  BurnTokens { burn_amt: Int }
}
