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

  let suites =
    Popper.suite
      [ ("take_string", take_string)
      ; ("peek_char", peek_char)
      ; ("peek_char_opt", peek_char_opt)
      ; ("peek_string", peek_string)
      ; ("any_char", any_char)
      ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
