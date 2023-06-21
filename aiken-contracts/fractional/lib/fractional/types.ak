/////////////////////

use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/math/rational.{Rational}
use aiken/transaction/credential.{PoolId, Script, VerificationKey}
use aiken/transaction/value.{Value}

// (555) pointer token prefix (NOT IMPLEMENTED IN CIP68)
pub const prefix_555 = #"0022bfb0"

// metadata
pub const metadata_key = #"6d65746164617461"

// minter
pub const minter_key = #"6d696e746572"

// pointer
pub const pointer_key = #"706f696e746572"

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

pub type RemoveSale =
  Void

pub type SaleRedeemer {
  Purchase
  StartSale { pointer: Value }
  ExtractSale { profit: Value }
  RemoveSale
}

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

// find a worker by job title inside the worker hot key dictionary.
pub fn find_worker(workers: Workers, job: ByteArray) -> PublicKeyHash {
  when dict.get(workers, job) is {
    Some(one) -> one
    None -> error @"worker not found"
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
  start_upper_bound: Int,
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
  bundle_amt: Int,
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

pub type Ratio {
  numerator: Int,
  denominator: Int,
}

pub fn ratio_to_rational(self: Ratio) -> Rational {
  expect Some(x) = rational.new(self.numerator, self.denominator)
  x
}

pub type OrderBookPrice {
  // pid and tkn of price
  want: BundleInfo,
  // price ratio
  ratio: Ratio,
  // plus or minus the slippage
  slippage: Ratio,
}

pub type OrderBookDatum {
  // who owns this utxo
  owner: OwnerInfo,
  // pid and tkn of what is being sold
  have: BundleInfo,
  // n haves for m wants
  price: OrderBookPrice,
  // the incentive for 1 tx
  incentive: TokenInfo,
}

pub fn valid_order_update(a: OrderBookDatum, b: OrderBookDatum) -> Bool {
  list.and([(a.owner == b.owner)?, (a.have == b.have)?])
}

// this funciton needs to be ran both ways, a,b and b,a, do this over the tx
pub fn is_ok_to_swap(a: OrderBookDatum, b: OrderBookDatum) -> Bool {
  let a_price = ratio_to_rational(a.price.ratio)
  let a_slip = ratio_to_rational(a.price.slippage)
  let b_price = ratio_to_rational(b.price.ratio)
  let a_low = rational.add(a_price, rational.negate(a_slip))
  let a_high = rational.add(a_price, a_slip)
  expect Some(b_inverse) = rational.reciprocal(b_price)
  list.and(
    [
      // no self swaps
      (a.owner != b.owner)?,
      // want I have is what you want
      (a.have == b.price.want)?,
      // want I want is what your have
      (a.price.want == b.have)?,
      // your price is is within my limits
      rational.compare_with(a_low, fn(l, r) { l <= r }, b_inverse)?,
      rational.compare_with(b_inverse, fn(l, r) { l <= r }, a_high)?,
    ],
  )
}

pub fn find_best(
  this_total: Rational,
  price: Rational,
  that_total: Rational,
) -> Int {
  let getting = rational.mul(this_total, price)
  let returning = rational.sub(that_total, getting)
  if
  list.or(
    [
      rational.compare(returning, rational.zero()) == Greater,
      rational.compare(returning, rational.zero()) == Equal,
    ],
  ){
  
    rational.floor(getting)
  } else {
    rational.floor(that_total)
  }
}

pub fn value_to_ratio(self: Value, token: BundleInfo) -> Rational {
  let amt = value.quantity_of(self, token.pid, token.tkn)
  rational.from_int(amt)
}

pub fn split_fee_value(self: Value) -> Value {
  let amt = value.quantity_of(self, value.ada_policy_id, value.ada_asset_name)
  if amt % 2 != 0 {
    error @"Even Fees Only"
  } else {
    value.from_lovelace(-amt / 2)
  }
}

pub type RemoveOrder =
  Void

pub type UpdateOrder =
  Void

pub type RefundOrder =
  Void

pub type TxId {
  id: ByteArray,
  idx: Int,
}

pub type OrderBookRedeemer {
  RemoveOrder
  UpdateOrder
  RefundOrder
  CompleteOrder(TxId)
}
