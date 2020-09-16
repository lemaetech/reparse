let () =
  Printexc.record_backtrace true ;
  Alcotest.run
    "reparse"
    [ ("track_lnum_cnum.ml", Track_lnum_cnum.suite)
    ; ("peek_next_return.ml", Peek_next_return.suite)
    ; ("infix.ml", Infix.suite) ]
