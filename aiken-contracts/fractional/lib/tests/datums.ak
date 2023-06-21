use aiken/list
use aiken/transaction/value.{Value}
use fractional/types.{
  BundleInfo, OrderBookDatum, OrderBookPrice, OwnerInfo, Ratio, SaleDatum,
  TokenInfo,
}

fn datum_a() -> SaleDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: Value = value.from_asset(#"acab", #"beef", 10)
  let max_bundle_size: Int = 10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

fn datum_c() -> SaleDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: Value = value.from_asset(#"", #"", 10)
  let max_bundle_size: Int = 10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

test good_purchase_datum() {
  types.purchase_datum_check(datum_a(), datum_a()) == True
}

test bad_purchase_datum() {
  types.purchase_datum_check(datum_a(), datum_c()) == False
}

fn order_datum_same(pkh: ByteArray) -> OrderBookDatum {
  let owner: OwnerInfo = OwnerInfo { pkh, sc: #"" }
  let have: BundleInfo = BundleInfo { pid: #"acab", tkn: #"beef" }
  let want: BundleInfo = BundleInfo { pid: #"acab", tkn: #"beef" }
  let p = Ratio { numerator: 1, denominator: 1 }
  let s = Ratio { numerator: 1, denominator: 1 }
  let price: OrderBookPrice = OrderBookPrice { want, ratio: p, slippage: s }
  let incentive: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 1 }
  OrderBookDatum { owner, have, price, incentive }
}

fn order_datum_a() -> OrderBookDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"acab", sc: #"" }
  let have: BundleInfo = BundleInfo { pid: #"acab", tkn: #"beef" }
  let want: BundleInfo = BundleInfo { pid: #"fade", tkn: #"face" }
  let p = Ratio { numerator: 1, denominator: 15 }
  let s = Ratio { numerator: 1, denominator: 4 }
  let price: OrderBookPrice = OrderBookPrice { want, ratio: p, slippage: s }
  let incentive: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 10 }
  OrderBookDatum { owner, have, price, incentive }
}

fn order_datum_b() -> OrderBookDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"beef", sc: #"" }
  let have: BundleInfo = BundleInfo { pid: #"fade", tkn: #"face" }
  let want: BundleInfo = BundleInfo { pid: #"acab", tkn: #"beef" }
  let p = Ratio { numerator: 14, denominator: 1 }
  let s = Ratio { numerator: 1, denominator: 1 }
  let price: OrderBookPrice = OrderBookPrice { want, ratio: p, slippage: s }
  let incentive: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 10 }
  OrderBookDatum { owner, have, price, incentive }
}

fn order_datum_c() -> OrderBookDatum {
  let owner: OwnerInfo = OwnerInfo { pkh: #"beef", sc: #"" }
  let have: BundleInfo = BundleInfo { pid: #"fade", tkn: #"face" }
  let want: BundleInfo = BundleInfo { pid: #"acab", tkn: #"beef" }
  let p = Ratio { numerator: 15, denominator: 6 }
  let s = Ratio { numerator: 1, denominator: 8 }
  let price: OrderBookPrice = OrderBookPrice { want, ratio: p, slippage: s }
  let incentive: TokenInfo = TokenInfo { pid: #"", tkn: #"", amt: 10 }
  OrderBookDatum { owner, have, price, incentive }
}

test good_swap() {
  let datum_a = order_datum_a()
  let datum_b = order_datum_b()
  list.and(
    [
      types.is_ok_to_swap(datum_a, datum_b),
      types.is_ok_to_swap(datum_b, datum_a),
    ],
  )
}

test bad_swap() {
  let datum_a = order_datum_a()
  let datum_c = order_datum_c()
  list.and(
    [
      !types.is_ok_to_swap(datum_a, datum_c),
      !types.is_ok_to_swap(datum_c, datum_a),
    ],
  )
}

test same_swap() {
  let datum_a = order_datum_same(#"acab")
  let datum_b = order_datum_same(#"beef")
  list.and(
    [
      types.is_ok_to_swap(datum_a, datum_b),
      types.is_ok_to_swap(datum_b, datum_a),
    ],
  )
}
