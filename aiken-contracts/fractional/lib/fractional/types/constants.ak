use aiken/hash.{Blake2b_224, Hash}
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
