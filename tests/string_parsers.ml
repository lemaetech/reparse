module Make_test (P : Test_parser.TEST_PARSER) = struct
  type int_result = (int, string) result [@@deriving show, ord, popper]

  type string_result = (string, string) result [@@deriving show, ord, popper]

  type string_opt_result = (string option, string) result
  [@@deriving show, ord, popper]

  open P.Infix

  let pos_test p pos inp =
    ( Format.sprintf "pos is %d" pos
    , Popper.(
        test (fun () ->
            let p = p *> P.pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let committed_pos_test p pos inp =
    ( Format.sprintf "committed_pos is %d" pos
    , Popper.(
        test (fun () ->
            let p = p *> P.committed_pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let to_string c = Format.sprintf "%c" c

  let empty () = P.of_string ""

  let peek_char =
    let p = P.peek_char >>= fun c -> P.string_of_chars [ c ] in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) )
        ; pos_test p 0 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal string_result_comparator (P.run p empty)
                  (Error "pos:0, n:1 eof")) )
        ])

  let peek_char_opt =
    let p =
      P.peek_char_opt
      >>| function
      | Some c -> Some (to_string c)
      | None -> None
    in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal string_opt_result_comparator (P.run p inp) (Ok (Some "h")))
          )
        ; pos_test p 0 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal string_opt_result_comparator (P.run p empty) (Ok None)) )
        ])

  let peek_string =
    let p = P.peek_string 5 in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is \"hello\""
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello")) )
        ; pos_test p 0 inp
        ; committed_pos_test p 0 inp
        ])

  let any_char =
    let p = P.any_char >>| to_string in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) )
        ; pos_test p 1 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal string_result_comparator (P.run p empty)
                  (Error "pos:0, n:1 eof")) )
        ])

  let char =
    let p = P.char 'h' >>| to_string in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) )
        ; pos_test p 1 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on 'c'"
          , test (fun () ->
                let p = P.char 'c' >>| to_string in
                equal string_result_comparator (P.run p inp)
                  (Error "[char] pos:0, expected 'c', got 'h'")) )
        ])

  let char_if =
    let p = P.char_if (fun c -> Char.equal 'h' c) >>| to_string in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is 'h'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) )
        ; pos_test p 1 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on 'c'"
          , test (fun () ->
                let p = P.char_if (fun c -> Char.equal 'c' c) >>| to_string in
                equal string_result_comparator (P.run p inp)
                  (Error "[char_if] pos:0 'h'")) )
        ])

  let string =
    let p = P.string "hello" in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello")) )
        ; pos_test p 5 inp
        ; committed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                let p = P.string "bye" in
                equal string_result_comparator (P.run p inp)
                  (Error "[string] \"bye\"")) )
        ])

  let string_of_chars =
    let p = P.string_of_chars [ 'h'; 'e'; 'l'; 'l'; 'o' ] in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal string_result_comparator (P.run p empty) (Ok "hello")) )
        ; pos_test p 0 empty
        ; committed_pos_test p 0 empty
        ])

  let take_string =
    let p = P.take_string 5 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( Format.sprintf "value is \"%s\"" "hello"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello")) )
        ; pos_test p 5 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                equal string_result_comparator (P.run p empty)
                  (Error "pos:0, n:5 eof")) )
        ])

  let cstruct_result_comparator =
    let cstruct =
      Popper.Comparator.make Cstruct.compare (fun fmt t ->
          Format.fprintf fmt "[%d,%d](%d)" t.off t.len
            (Bigarray_compat.Array1.dim t.buffer))
    in
    Popper.Comparator.(result ~ok:cstruct ~error:string)

  let take_cstruct =
    let p = P.take_cstruct 5 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal cstruct_result_comparator (P.run p inp)
                  (Ok (Cstruct.of_string "hello"))) )
        ; pos_test p 5 inp
        ; committed_pos_test p 0 inp
        ; ( "fail on eof"
          , test (fun () ->
                let p = P.take_cstruct 12 in
                equal cstruct_result_comparator (P.run p inp)
                  (Error "pos:0, n:12 eof")) )
        ])

  let unsafe_take_cstruct =
    let p = P.unsafe_take_cstruct 5 in
    let inp () = P.of_string "hello world" in
    let p2 =
      let p1 = P.unsafe_take_cstruct 6 in
      let p2 = P.take_cstruct 5 in
      P.map2 (fun a b -> Cstruct.append a b) p1 p2
    in
    Popper.(
      suite
        [ ( "value is 'hello'"
          , test (fun () ->
                equal cstruct_result_comparator (P.run p inp)
                  (Ok (Cstruct.of_string "hello"))) )
        ; pos_test p 5 inp
        ; committed_pos_test p 5 inp
        ; ( "fail"
          , test (fun () ->
                let p = P.unsafe_take_cstruct 12 in
                equal cstruct_result_comparator (P.run p inp)
                  (Error "pos:0, n:12 eof")) )
        ; ( "value is 'hello world'"
          , test (fun () ->
                equal cstruct_result_comparator (P.run p2 inp)
                  (Ok (Cstruct.of_string "hello world"))) )
        ; pos_test p2 11 inp
        ; committed_pos_test p2 6 inp
        ])

  let suites =
    Popper.suite
      [ ("peek_char", peek_char)
      ; ("peek_char_opt", peek_char_opt)
      ; ("peek_string", peek_string)
      ; ("any_char", any_char)
      ; ("char", char)
      ; ("any_char", any_char)
      ; ("char_if", char_if)
      ; ("string", string)
      ; ("string_of_chars", string_of_chars)
      ; ("take_string", take_string)
      ; ("take_cstruct", take_cstruct)
      ; ("unsafe_take_cstruct", unsafe_take_cstruct)
      ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
