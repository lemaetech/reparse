let () = Printexc.record_backtrace true

let () =
  Popper.suite
    [ ("String parsers", Test_string_parsers.suite)
    ; ("Alternative parsers", Test_alternative_parsers.suite)
    ; ("Boolean parsers", Test_boolean_parsers.suite)
    ; ("Input parsers", Test_input_parsers.suite) ]
  |> Popper.run
