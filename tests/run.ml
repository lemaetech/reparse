let () = Printexc.record_backtrace true

let () =
  Popper.suite
    [
      ("String", Test_string_parsers.suite);
      ("Alternative", Test_alternative_parsers.suite);
      ("Boolean", Test_boolean_parsers.suite);
      ("Input", Test_input_parsers.suite);
    ]
  |> Popper.run
