module type PARSER = Parser_sig.S
module type INPUT = Input.S

module Make = Parser.Make
module String_input = Input.String
module File_input = Input.File
module String_parser = Make (String_input)
module File_parser = Make (File_input)
