use fractional_sale/datums.{Datum, OwnerInfo, TokenInfo}

fn datum_a() -> Datum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: TokenInfo =
    TokenInfo { pid: #"acab", tkn: #"beef", amt: 10 }
  Datum { owner, bundle, cost }
}

fn datum_c() -> Datum {
  let owner: OwnerInfo =
    OwnerInfo { pkh: #"", sc: #"" }
  let bundle: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  let cost: TokenInfo =
    TokenInfo { pid: #"", tkn: #"", amt: 10 }
  Datum { owner, bundle, cost }
}

test good_purchase_datum() {
  datums.purchase_datum_check(datum_a(), datum_a()) == True
}

test bad_purchase_datum() {
  datums.purchase_datum_check(datum_a(), datum_c()) == False
}
