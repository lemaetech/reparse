let () =
  Popper.suite
    [ ("String parsers", Test_string_parsers.suite)
    ; ("Alternative parsers", Test_alternative_parsers.suite)
    ]
  |> Popper.run
