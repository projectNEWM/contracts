use assist/prefixes
use assist/values

test token_name() {
  let tkn =
    values.unique_token_name(
      #"ab69aab2efe96149ca2fc045f8cdfadaa2213e5f71f7f427632ac1216bd6106d",
      0,
      prefixes.prefix_100,
    )
  tkn == #"000643b00001f1ca8bb6ed0bd798019448bf8b5b9a539958477b53fd86c6d27e"
}
