pub type BuyAmount {
  amt: Int,
}

pub type Remove =
  Void

pub type Update =
  Void

pub type Redeemer {
  Purchase { buy_amt: BuyAmount }
  Update
  Remove
}

const max_bundle_size = 10

pub fn valid_bundle_size(buy_amt: BuyAmount) -> Bool {
  buy_amt.amt > 0 && buy_amt.amt <= max_bundle_size
}
