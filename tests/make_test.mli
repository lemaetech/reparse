val make
  :  string
  -> (('a Reparse.Parser.t -> 'a) -> 'b)
  -> string
  -> (string * [> `Quick ] * 'b) list
