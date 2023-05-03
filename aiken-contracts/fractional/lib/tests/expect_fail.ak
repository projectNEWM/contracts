use aiken/builtin

test expect_list_pass() {
  let initial_car =
    builtin.list_data([builtin.i_data(5), builtin.i_data(6), builtin.i_data(7)])
  expect [a, ..d]: List<Int> =
    initial_car
  a == 5 && d == [6, 7]
}

test expect_list_fail() {
  let bad_data =
    builtin.b_data(#"")
  expect [a, ..d]: List<Int> =
    bad_data
  a == 5 && d == [6, 7]
}
