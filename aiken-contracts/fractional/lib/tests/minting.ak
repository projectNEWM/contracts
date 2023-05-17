use assist/values

test token_name() {
  let tkn =
    values.unique_token_name(
      #"82fca2f3221cf2d3ef017e8aa76f5c70317df0af32d327c84af4b9b9bddad91f",
      0,
      values.prefix_100,
    )
  tkn == #"2831303029008fc36029569576a4a3c05596ce7cd1370da0ffc282e510062e8f"
  // 2831303029008fc36029569576a4a3c05596ce7cd1370da0ffc282e510062e8f
  // 2831303029008fc36029569576a4a3c05596ce7cd1370da0ffc282e510062e8f41
}
