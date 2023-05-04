use aiken/transaction.{InlineDatum, Input, Output}

pub fn find_inbound_datum(possible_input: Input) -> Data {
  when possible_input.output.datum is {
    InlineDatum(inbound_datum) ->
      inbound_datum
    _ ->
      error @"expected inbound inline datum"
  }
}

pub fn find_outbound_datum(possible_output: Output) -> Data {
  when possible_output.datum is {
    InlineDatum(outbound_datum) ->
      outbound_datum
    _ ->
      error @"expected outbound inline datum"
  }
}
