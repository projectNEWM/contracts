use fractional/types.{BuyAmount}

fn buy_amt_a() -> BuyAmount {
  BuyAmount { amt: 5 }
}

fn buy_amt_b() -> BuyAmount {
  BuyAmount { amt: 15 }
}

test good_bundle_size() {
  types.valid_bundle_size(buy_amt_a()) == True
}

test bad_bundle_size() {
  types.valid_bundle_size(buy_amt_b()) == False
}
