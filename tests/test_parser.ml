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

  type int_opt_result = (int option, string) result
  [@@deriving show, ord, popper]

  open P.Infix

  let pos_test p pos inp =
    ( Format.sprintf "pos is %d" pos
    , Popper.(
        test (fun () ->
            let p = p *> P.pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let last_trimmed_pos_test p pos inp =
    ( Format.asprintf "last_trimmed_pos is %d" pos
    , Popper.(
        test (fun () ->
            let p = p *> P.last_trimmed_pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let buffer_size_test p sz inp =
    let pp_int_opt fmt =
      Format.pp_print_option
        ~none:(fun fmt () -> Format.fprintf fmt "'None'")
        Format.pp_print_int fmt
    in
    ( Format.asprintf "buffer size is %a" pp_int_opt sz
    , Popper.(
        test (fun () ->
            let p = p *> P.input_buffer_size in
            equal int_opt_result_comparator (P.run p inp) (Ok sz))) )

  let empty () = P.of_string ""
end
