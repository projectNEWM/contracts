use aiken/bytearray
use aiken/transaction/credential.{PoolId}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use assist/types/hashes.{PublicKeyHash, ValidatorHash}
use assist/types/wallet.{Wallet}

// staking info
pub type Staking {
  pool_id: PoolId,
  reward_wallet: Wallet,
}

// keepers are like admins
pub type Keepers {
  pkhs: List<PublicKeyHash>,
  threshold: Int,
}

/// All keepers must have valid length public key hashes. There must be at
/// at least one keeper and the threshold must be logical with respect to the
/// list of keepers.
///
pub fn is_valid_keepers(keepers: Keepers) -> Bool {
  do_is_valid_keepers(keepers.pkhs, 0, keepers.threshold)
}

fn do_is_valid_keepers(
  keepers: List<PublicKeyHash>,
  counter: Int,
  threshold: Int,
) -> Bool {
  when keepers is {
    [] -> and {
        counter > 0,
        counter >= threshold,
      }
    [pkh, ..pkhs] ->
      if bytearray.length(pkh) == 28 {
        // valid length count it
        do_is_valid_keepers(pkhs, counter + 1, threshold)
      } else {
        // not a valid pkh
        False
      }
  }
}

// contract validator hashes
pub type Contracts {
  cip68: ValidatorHash,
  sale: ValidatorHash,
  queue: ValidatorHash,
  band: ValidatorHash,
  stake: ValidatorHash,
}

/// All the contract validator hashes must be length 28 or empty.
pub fn is_valid_contracts(contracts: Contracts) -> Bool {
  and {
    or {
      bytearray.is_empty(contracts.cip68),
      bytearray.length(contracts.cip68) == 28,
    },
    or {
      bytearray.is_empty(contracts.sale),
      bytearray.length(contracts.sale) == 28,
    },
    or {
      bytearray.is_empty(contracts.queue),
      bytearray.length(contracts.queue) == 28,
    },
    or {
      bytearray.is_empty(contracts.band),
      bytearray.length(contracts.band) == 28,
    },
    or {
      bytearray.is_empty(contracts.stake),
      bytearray.length(contracts.stake) == 28,
    },
  }
}

pub type Fees {
  purchase_queue_bound: Int,
  refund_queue_bound: Int,
  start_sale_bound: Int,
}

/// The tx fee must be less than equal to the upper bound.
///
pub fn valid_tx_fee(fee: Value, upper_bound: Int) -> Bool {
  value.lovelace_of(fee) <= upper_bound
}

pub type BatcherData {
  monster_pid: PolicyId,
  monster_prefixes: List<ByteArray>,
  batcher_pid: PolicyId,
  batcher_tkn: AssetName,
}

// data to be used inside other contracts
pub type ReferenceDatum {
  hot_key: PublicKeyHash,
  keepers: Keepers,
  staking: Staking,
  contracts: Contracts,
  fees: Fees,
  pointer: PolicyId,
  batcher_data: BatcherData,
}
