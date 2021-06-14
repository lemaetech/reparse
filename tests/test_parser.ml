module type TEST_PARSER = sig
  include Reparse.PARSER

  val of_string : string -> input

  val run : 'a t -> (unit -> input) -> ('a, string) result
end

module String : TEST_PARSER = struct
  include Reparse.String

  let of_string = input_of_string

  let run p i = parse p (i ())
end

module Lwt : TEST_PARSER = struct
  include Reparse_lwt.Stream

  let of_string s = input_of_stream (Lwt_stream.of_string s)

  let run p inp = Lwt_main.run (parse p @@ inp ())
end

module Make_helper (P : TEST_PARSER) = struct
  type int_result = (int, string) result [@@deriving show, ord, popper]

  type string_result = (string, string) result [@@deriving show, ord, popper]

  type string_opt_result = (string option, string) result
  [@@deriving show, ord, popper]

  type char_result = (char, string) result [@@deriving show, ord, popper]

  type char_opt_result = (char option, string) result
  [@@deriving show, ord, popper]

  type unit_result = (unit, string) result [@@deriving show, ord, popper]

  type bool_result = (bool, string) result [@@deriving show, ord, popper]

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

  (* let to_string c = Format.sprintf "%c" c *)

  let empty () = P.of_string ""
end
