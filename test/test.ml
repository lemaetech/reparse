let () =
  Printexc.record_backtrace true ;
  Alcotest.run
    "reparse"
    [ ("track_lnum_cnum.ml", Track_lnum_cnum.suite)
    ; ("peek_next_return.ml", Peek_next_return.suite)
    ; ("infix.ml", Infix.suite)
    ; ("char_string_sat_line.ml", Char_string_sat_line.suite)
    ; ("skip_skip_while.ml", Skip_skip_while.suite) ]
