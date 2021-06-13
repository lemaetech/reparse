module type TEST_PARSER = sig
  include Reparse.PARSER

  val of_string : string -> input

  val run : 'a t -> input -> ('a, string) result
end

module String_parser_tester : TEST_PARSER = struct
  include Reparse.String

  let of_string = input_of_string

  let run = parse
end

module Lwt_parser_tester : TEST_PARSER = struct
  include Reparse_lwt.Stream

  let of_string s = input_of_stream (Lwt_stream.of_string s)

  let run p inp = Lwt_main.run (parse p inp)
end
