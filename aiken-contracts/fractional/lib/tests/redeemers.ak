use fractional/types.{BuyAmount}

fn buy_a() -> BuyAmount {
  BuyAmount { amt: 5 }
}

fn buy_b() -> BuyAmount {
  BuyAmount { amt: 15 }
}

test good_bundle_size() {
  let max_bundle_size: Int = 10
  types.valid_number_of_bundles(buy_a().amt, max_bundle_size) == True
}

test bad_bundle_size() {
  let max_bundle_size: Int = 10
  types.valid_number_of_bundles(buy_b().amt, max_bundle_size) == False
}
