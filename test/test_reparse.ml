let () =
  Printexc.record_backtrace true ;
  Alcotest.run
    "reparse"
    [ ("track_lnum_cnum.ml", Track_lnum_cnum.suite)
    ; ("peek.ml", Peek.suite)
    ; ("next", Next.suite)
    ; ("return.ml", Return.suite)
    ; ("infix.ml", Infix.suite) ]
