use aiken/hash.{Blake2b_224, Blake2b_256, Hash}
use aiken/transaction.{Transaction}
use aiken/transaction/credential.{Script, VerificationKey}

// batcher token
pub const batcher_token_name = #"5ca1ab1e000affab1e000ca11ab1e0005e77ab1e"

pub type Lovelace =
  Int

// wallet hash
pub type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

// script hash
pub type ValidatorHash =
  Hash<Blake2b_224, Script>

pub type TransactionIdHash =
  Hash<Blake2b_256, Transaction>
