module Parser = Parser
module Parser_sig = Parser_sig
module Input = Input
module String_parser = Parser.Make (Input.String)
module File_parser = Parser.Make (Input.File)
