module Make_test (P : Test_parser.TEST_PARSER) = struct
  open Test_parser.Make_helper (P)
  open P.Infix

  let peek_char =
    let p = P.peek_char in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () -> equal char_result_comparator (P.run p inp) (Ok 'h'))
          )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal char_result_comparator (P.run p empty)
                  (Error "[peek_char] pos:0 eof") ) ) ])

  let peek_char_opt =
    let p = P.peek_char_opt in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal char_opt_result_comparator (P.run p inp) (Ok (Some 'h')) )
          )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal char_opt_result_comparator (P.run p empty) (Ok None) ) )
        ])

  let peek_string =
    let p = P.peek_string 5 in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is \"hello\""
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello") ) )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp ])

  let any_char =
    let p = P.any_char in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () -> equal char_result_comparator (P.run p inp) (Ok 'h'))
          )
        ; pos_test p 1 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal char_result_comparator (P.run p empty)
                  (Error "[any_char] pos:0 eof") ) ) ])

  let any_char_unbuffered =
    let p = P.unsafe_any_char in
    let inp () = P.of_string "hello" in
    let p2 =
      (P.unsafe_any_char, P.unsafe_any_char, P.unsafe_any_char)
      <$$$> (fun c1 c2 c3 -> List.to_seq [c1; c2; c3] |> String.of_seq)
      >>= fun s -> P.trim_input_buffer $> s in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () -> equal char_result_comparator (P.run p inp) (Ok 'h'))
          )
        ; pos_test p 1 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal char_result_comparator (P.run p empty)
                  (Error "[unsafe_any_char] pos:0 eof") ) )
        ; ( {|value is "hel"|}
          , test (fun () ->
                equal string_result_comparator (P.run p2 inp) (Ok "hel") ) )
        ; pos_test p2 3 inp
        ; last_trimmed_pos_test p2 3 inp ])

  let char =
    let p = P.char 'h' in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () -> equal char_result_comparator (P.run p inp) (Ok 'h'))
          )
        ; pos_test p 1 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on 'c'"
          , test (fun () ->
                let p = P.char 'c' in
                equal char_result_comparator (P.run p inp)
                  (Error "[char] pos:0, expected 'c', got 'h'") ) ) ])

  let char_if =
    let p = P.char_if (fun c -> Char.equal 'h' c) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () -> equal char_result_comparator (P.run p inp) (Ok 'h'))
          )
        ; pos_test p 1 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on 'c'"
          , test (fun () ->
                let p = P.char_if (fun c -> Char.equal 'c' c) in
                equal char_result_comparator (P.run p inp)
                  (Error "[char_if] pos:0 'h'") ) ) ])

  let string_cs =
    let p = P.string_cs "hello" in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello") ) )
        ; pos_test p 5 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                let p = P.string_cs "bye" in
                equal string_result_comparator (P.run p inp)
                  (Error "[string_cs] \"bye\"") ) )
        ; ( "case sensitive"
          , test (fun () ->
                let inp () = P.of_string "HELLO world" in
                equal string_result_comparator (P.run p inp)
                  (Error {|[string_cs] "hello"|}) ) ) ])

  let string_ci =
    let p = P.string_ci "hello" in
    let inp () = P.of_string "HELLO world" in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello") ) )
        ; pos_test p 5 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                let p = P.string_ci "bye" in
                equal string_result_comparator (P.run p inp)
                  (Error "[string_ci] \"bye\"") ) )
        ; ( "case sensitive"
          , test (fun () ->
                let inp () = P.of_string "hello world" in
                equal string_result_comparator (P.run p inp) (Ok "hello") ) ) ])

  let string_of_chars =
    let p = P.string_of_chars ['h'; 'e'; 'l'; 'l'; 'o'] in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal string_result_comparator (P.run p empty) (Ok "hello") ) )
        ; pos_test p 0 empty
        ; last_trimmed_pos_test p 0 empty ])

  let take_string =
    let p = P.take_string 5 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( Format.sprintf "value is \"%s\"" "hello"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello") ) )
        ; pos_test p 5 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal string_result_comparator (P.run p empty)
                  (Error "pos:0, n:5 eof") ) ) ])

  let cstruct_result_comparator =
    let cstruct =
      Popper.Comparator.make Cstruct.compare (fun fmt t ->
          Format.fprintf fmt "[%d,%d](%d)" t.off t.len
            (Bigarray_compat.Array1.dim t.buffer) ) in
    Popper.Comparator.(result ~ok:cstruct ~error:string)

  let take_cstruct =
    let p = P.take_cstruct 5 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal cstruct_result_comparator (P.run p inp)
                  (Ok (Cstruct.of_string "hello")) ) )
        ; pos_test p 5 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                let p = P.take_cstruct 12 in
                equal cstruct_result_comparator (P.run p inp)
                  (Error "pos:0, n:12 not enough input") ) ) ])

  let suites =
    Popper.suite
      [ ("peek_char", peek_char)
      ; ("peek_char_opt", peek_char_opt)
      ; ("peek_string", peek_string)
      ; ("any_char", any_char)
      ; ("any_char_unbuffered", any_char_unbuffered)
      ; ("char", char)
      ; ("char_if", char_if)
      ; ("string_cs", string_cs)
      ; ("string_ci", string_ci)
      ; ("string_of_chars", string_of_chars)
      ; ("take_string", take_string)
      ; ("take_cstruct", take_cstruct) ]
end

let suite =
  let module String = Make_test (Test_parser.String) in
  let module Lwt_stream = Make_test (Test_parser.Lwt_stream) in
  let module Lwt_fd = Make_test (Test_parser.Lwt_fd) in
  let module Lwt_channel = Make_test (Test_parser.Lwt_channel) in
  Popper.suite
    [ ("Reparse.String", String.suites)
    ; ("Reparse_lwt.Stream", Lwt_stream.suites)
    ; ("Reparse_lwt_unix.Fd", Lwt_fd.suites)
    ; ("Reparse_lwt_unix.Channel", Lwt_channel.suites) ]
