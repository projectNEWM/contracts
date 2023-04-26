use fractional/types.{OwnerInfo, SaleDatum, TokenInfo}

fn datum_a() -> SaleDatum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: TokenInfo =
    TokenInfo { pid: #"acab", tkn: #"beef", amt: 10 }
  let max_bundle_size: Int =
    10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

fn datum_c() -> SaleDatum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let max_bundle_size: Int =
    10
  SaleDatum { owner, bundle, cost, max_bundle_size }
}

test good_purchase_datum() {
  types.purchase_datum_check(datum_a(), datum_a()) == True
}

test bad_purchase_datum() {
  types.purchase_datum_check(datum_a(), datum_c()) == False
}
