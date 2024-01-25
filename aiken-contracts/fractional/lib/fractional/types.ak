/////////////////////

use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash}
use aiken/math/rational.{Rational}
use aiken/transaction.{Input, OutputReference}
use aiken/transaction/credential.{PoolId, Script, VerificationKey}
use aiken/transaction/value.{Value}

// pointer token prefix
pub const pointer_prefix = #"a110ca7ab1e000"

// metadata
pub const metadata_key = #"6d65746164617461"

// minter
pub const minter_key = #"6d696e746572"

// pointer
pub const pointer_key = #"706f696e746572"

// batcher token
pub const batcher_token_name = #"5ca1ab1e000affab1e000ca11ab1e0005e77ab1e"

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

pub type UpdateSale =
  Void

pub type SaleRedeemer {
  Purchase
  StartSale { pointer: TokenInfo }
  ExtractSale { profit: Value }
  RemoveSale
  UpdateSale { lovelace: Int }
}

pub fn valid_sale_datum_update(old: SaleDatum, new: SaleDatum) -> Bool {
  and {
    // the owner can't change
    old.owner == new.owner,
    // can't change bundle pid or tkn
    old.bundle.pid == new.bundle.pid,
    old.bundle.tkn == new.bundle.tkn,
    // can't set the bundle size to zero
    new.bundle.amt > 0,
    // the cost can't be zero (empty dict)
    new.cost != value.zero(),
    // the max bundle size can't be zero
    new.max_bundle_size > 0,
  }
}

// greater than zero and less than or equal to the max bundle size
pub fn valid_number_of_bundles(buy_amt: Int, max_bundle_size: Int) -> Bool {
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
    None -> fail @"worker not found"
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
  band: ValidatorHash,
  stake: ValidatorHash,
}

pub type FeeInfoData {
  purchase_queue_bound: Int,
  refund_queue_bound: Int,
  start_sale_bound: Int,
  purchase_order_bound: Int,
  refund_order_bound: Int,
}

pub type BatcherData {
  monster_pid: ByteArray,
  // these are the monster prefix token names
  monster_tkns: List<ByteArray>,
  batcher_pid: ByteArray,
  batcher_tkn: ByteArray,
}

/// Token names are from the value or total value. Monster token names are the
/// official prefixes for the monster nfts.
///
pub fn has_tokens(
  token_names: List<ByteArray>,
  monster_tkns: List<ByteArray>,
) -> Bool {
  when monster_tkns is {
    // I found all of them
    [] -> True
    // go through all the prefixes from the set
    [prefix, ..prefixes] ->
      if find_token_name_from_prefix(token_names, prefix) == True {
        // found something so go to the next one
        has_tokens(token_names, prefixes)
      } else {
        // missing something from the set
        // or something that isn't part of the set
        False
      }
  }
}

fn find_token_name_from_prefix(token_names: List<ByteArray>, prefix: ByteArray) {
  when token_names is {
    // didn't find a token name with the correct prefix
    [] -> False
    // check token name if prefix exist
    [tkn, ..tkns] ->
      if prefix_in_token_name(prefix, tkn) == True {
        True
      } else {
        find_token_name_from_prefix(tkns, prefix)
      }
  }
}

/// Check if some known prefix exists within some token name.
fn prefix_in_token_name(prefix: ByteArray, token_name: ByteArray) -> Bool {
  // the length of the prefix counting from 0
  let length_of_prefix = bytearray.length(prefix) - 1
  // excluding the number does at least the prefix match
  bytearray.slice(token_name, start: 0, end: length_of_prefix) == prefix
}

// data to be used inside other contracts
pub type ReferenceDatum {
  hot_key: Workers,
  keepers: KeeperData,
  staking: StakePoolData,
  contracts: ContractHashData,
  fee_info: FeeInfoData,
  pointer_pid: ByteArray,
  batcher_info: BatcherData,
}

// The tx fee must be less than equal to the upper bound
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

pub type UpdateBatcher =
  Void

pub type ReferenceRedeemer {
  UpdateHotKey
  UpdateKeepers
  UpdateStaking
  UpdateContracts
  UpdateFeeInfo
  UpdatePointer
  UpdateBatcher
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

pub type QueueCancel =
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
  bundle: BundleInfo,
  number_of_bundles: Int,
  incentive: TokenInfo,
  pointer_tkn: ByteArray,
}

pub fn prove_existence_of_incentive(
  validating_value: Value,
  the_incentive: TokenInfo,
) -> Bool {
  and {
    // The incentive must exist
    value.quantity_of(validating_value, the_incentive.pid, the_incentive.tkn) >= the_incentive.amt,
    // the amount must be greater than zero
    the_incentive.amt > 0,
  }
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
  // minimum order thresholds
  have_threshold: Int,
  want_threshold: Int,
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
  and {
    (a.owner == b.owner)?,
    (a.have == b.have)?,
  }
}

// this funciton needs to be ran both ways, a,b and b,a, do this over the tx
pub fn is_ok_to_swap(a: OrderBookDatum, b: OrderBookDatum) -> Bool {
  let a_price = ratio_to_rational(a.price.ratio)
  let a_slip = ratio_to_rational(a.price.slippage)
  let b_price = ratio_to_rational(b.price.ratio)
  let a_low = rational.sub(a_price, a_slip)
  let a_high = rational.add(a_price, a_slip)
  expect Some(b_inverse) = rational.reciprocal(b_price)
  and {
    // no self swaps
    a.owner != b.owner,
    // want I have is what you want
    a.have == b.price.want,
    // want I want is what you have
    a.price.want == b.have,
    // your price is is within my limits
    rational.compare_with(a_low, fn(l, r) { l <= r }, b_inverse)?,
    rational.compare_with(b_inverse, fn(l, r) { l <= r }, a_high)?,
  }
}

pub fn find_best(
  this_total: Rational,
  price: Rational,
  that_total: Rational,
) -> Int {
  let getting = rational.mul(this_total, price)
  let returning = rational.sub(that_total, getting)
  if or {
    rational.compare(returning, rational.zero()) == Greater,
    rational.compare(returning, rational.zero()) == Equal,
  } {
    // there is more or exactly zero of the getting token inside that_total
    rational.floor(getting)
  } else {
    // there is only enough for that_total
    rational.floor(that_total)
  }
}

pub fn value_to_rational(self: Value, token: BundleInfo) -> Rational {
  let amt = value.quantity_of(self, token.pid, token.tkn)
  rational.from_int(amt)
}

pub fn split_fee_value(self: Value) -> Int {
  let amt = value.quantity_of(self, value.ada_policy_id, value.ada_asset_name)
  if amt % 2 != 0 {
    fail @"Even Fees Only"
  } else {
    // just return the int
    -amt / 2
  }
}

pub fn ratio_average(a: Ratio, b: Ratio) -> Ratio {
  // geometric mean
  Ratio {
    numerator: sqrt(a.numerator * b.denominator),
    denominator: sqrt(a.denominator * b.numerator),
  }
}

pub fn sqrt(number: Int) -> Int {
  sqrt_babylonian(number, number, ( number + 1 ) / 2)
}

/// this is great for small numbers less than about 3^28
/// after that the Bakhshali method is better
fn sqrt_babylonian(number: Int, x: Int, y: Int) -> Int {
  if y >= x {
    x
  } else if number < 0 {
    fail @"Number Must Be Positive"
  } else if number == 0 {
    0
  } else {
    sqrt_babylonian(number, y, ( y + number / y ) / 2)
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

// We need some data types for the new monster batcher nft pass
pub type BandLockUpDatum {
  owner: OwnerInfo,
  full_flag: Int,
}

pub fn set_full_flag(old: BandLockUpDatum, new: BandLockUpDatum) -> Bool {
  and {
    old.owner == new.owner,
    old.full_flag == 0,
    new.full_flag == 1,
  }
}

pub type MintBand =
  Void

pub type BurnBand =
  Void

pub type BandLockUpRedeemer {
  RemoveBand
  AddToBand { member: Value }
  MintBand
  BurnBand
}

pub type BatcherTokenRedeemer {
  MintBatcherToken
  BurnBatcherToken
}

pub fn prove_existence_of_batcher(
  pid: ByteArray,
  tkn: ByteArray,
  inputs: List<Input>,
  queue_out_ref: OutputReference,
  sale_out_ref: OutputReference,
) -> Bool {
  when inputs is {
    [input, ..rest] ->
      // cant be queue or sale and contains at least 1
      if and {
        input.output_reference != queue_out_ref,
        input.output_reference != sale_out_ref,
        value.quantity_of(input.output.value, pid, tkn) >= 1,
      } {
        True
      } else {
        prove_existence_of_batcher(pid, tkn, rest, queue_out_ref, sale_out_ref)
      }
    // nothing was found
    [] -> False
  }
}
