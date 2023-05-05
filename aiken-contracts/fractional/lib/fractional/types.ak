use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction/credential.{PoolId, Script, VerificationKey}
use aiken/transaction/value.{Value}

// (100) reference token prefix
pub const prefix_100 =
  #"2831303029"

// (222) non-fungible token prefix
pub const prefix_222 =
  #"2832323229"

// (333) fungible token prefix
pub const prefix_333 =
  #"2833333329"

// (444) semi-fungible token prefix
pub const prefix_444 =
  #"2834343429"

// (555) pointer token prefix (NOT IMPLEMENTED IN CIP68)
pub const prefix_555 =
  #"2835353529"

// metadata
pub const metadata_key =
  #"6d65746164617461"

// minter
pub const minter_key =
  #"6d696e746572"

// pointer
pub const pointer_key =
  #"706f696e746572"

// wallet hash
pub type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

// script hash
pub type ValidatorHash =
  Hash<Blake2b_224, Script>

// https://cips.cardano.org/cips/cip68/
pub type CIP68Datum {
  metadata: Dict<Data, Data>,
  version: Int,
}

// Removes
pub type RemoveMeta =
  Void

// increases the lovelace on an utxo
pub type LovelaceAmount {
  increase: Int,
}

// only can remove or update
pub type CIP68Redeemer {
  RemoveMeta
  UpdateMeta { lovelace: LovelaceAmount, direction: Int }
}

// keep this as a data struc so we can add more later
pub type BuyAmount {
  amt: Int,
}

pub type Purchase =
  Void

pub type UpdateSale =
  Void

pub type RemoveSale =
  Void

pub type SaleRedeemer {
  Purchase
  UpdateSale
  RemoveSale
}

// hardcoded max bundle size
// const max_bundle_size =
//   10

// greater than zero and less than or equal to the max bundle size
pub fn valid_bundle_size(buy_amt: Int, max_bundle_size: Int) -> Bool {
  buy_amt > 0 && buy_amt <= max_bundle_size
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

// single token bundle and cost
pub type SaleDatum {
  owner: OwnerInfo,
  bundle: TokenInfo,
  cost: Value,
  max_bundle_size: Int,
}

// simple datum equality
pub fn purchase_datum_check(
  inbound_datum: SaleDatum,
  outbound_datum: SaleDatum,
) -> Bool {
  inbound_datum == outbound_datum
}

pub type Workers =
  Dict<ByteArray, PublicKeyHash>

pub fn find_worker(workers: Workers, job: ByteArray) -> PublicKeyHash {
  when dict.get(workers, job) is {
    Some(one) ->
      one
    None ->
      error @"worker not found"
  }
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
  queue: ValidatorHash,
  stake: ValidatorHash,
}

pub type FeeInfoData {
  purchase_upper_bound: Int,
  refund_upper_bound: Int,
}

// data to be used inside other contracts
pub type ReferenceDatum {
  hot_key: Workers,
  keepers: KeeperData,
  staking: StakePoolData,
  contracts: ContractHashData,
  fee_info: FeeInfoData,
  pointer_pid: ByteArray,
}

// The tx fee must be less than the upper bound
pub fn valid_tx_fee(fee: Value, upper_bound: Int) -> Bool {
  value.lovelace_of(fee) <= upper_bound
}

pub type UpdateHotKey =
  Void

pub type UpdateKeepers =
  Void

pub type UpdateStaking =
  Void

pub type UpdateContracts =
  Void

pub type UpdateFeeInfo =
  Void

pub type UpdatePointer =
  Void

pub type ReferenceRedeemer {
  UpdateHotKey
  UpdateKeepers
  UpdateStaking
  UpdateContracts
  UpdateFeeInfo
  UpdatePointer
}

pub type StakeData {
  stake_cred: ValidatorHash,
}

pub type WithdrawRedeemer {
  Withdraw
  Delegate(StakeData)
}

pub type MintRedeemer {
  MintTokens
  BurnTokens { burn_amt: Int }
}

pub type QueuePurchase =
  Void

/// Queue things
pub type QueueRedeemer {
  QueuePurchase
  QueueRefund
  QueueCancel
}

pub type BundleInfo {
  pid: ByteArray,
  tkn: ByteArray,
}

// single token bundle and cost
pub type QueueDatum {
  owner: OwnerInfo,
  pointer: BundleInfo,
  amt: Int,
  incentive: TokenInfo,
  pointer_tkn: ByteArray,
}

pub fn prove_existence_of_incentive(
  validating_value: Value,
  the_incentive: TokenInfo,
) -> Bool {
  list.and(
    [
      // The incentive must exist
      value.quantity_of(validating_value, the_incentive.pid, the_incentive.tkn) >= the_incentive.amt,
      // the amount cant be zero
      the_incentive.amt != 0,
    ],
  )
}
