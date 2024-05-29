use assist/types/token.{Token, Tokens}
use assist/types/wallet.{Wallet}

// the sale datum
pub type SaleDatum {
  owner: Wallet,
  bundle: Token,
  cost: Token,
  max_bundle_size: Int,
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
    // the cost can't be zero
    new.cost.amt > 0,
    // the max bundle size can't be zero
    new.max_bundle_size > 0,
  }
}

// greater than zero and less than or equal to the max bundle size
pub fn valid_number_of_bundles(buy_amt: Int, max_bundle_size: Int) -> Bool {
  and {
    // must be buying something
    buy_amt > 0,
    // must be less than or equal to the max bundle size
    buy_amt <= max_bundle_size,
  }
}

pub type SaleRedeemer {
  Purchase
  StartSale { pointer: Token }
  TransformSale { transforms: Tokens }
  RemoveSale
}
