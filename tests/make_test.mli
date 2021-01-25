val make
  :  string
  -> (('a Reparse.t -> 'a) -> 'b)
  -> string
  -> (string * [> `Quick ] * 'b) list
