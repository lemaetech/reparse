type 'a t = input -> pos -> 'a success -> await -> failure -> unit

and input = Cstruct.t

and pos = int

and 'a parse =
  [ `Ok of pos * 'a
  | `Fail of pos * string list * string
  | `Awaiting_input of pos ]

and failure = pos -> string list -> string -> unit

and 'a success = pos -> 'a -> unit

and await = pos -> unit

let return v : 'a t = fun _input pos succ _await _fail -> succ pos v

let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
 fun p f input pos succ await fail ->
  let succ' pos' v = (f v) input pos' succ await fail in
  p input pos succ' await fail

let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
 fun p f input pos succ await fail ->
  let succ' pos' v = succ pos' (f v) in
  p input pos succ' await fail

let ( *> ) : _ t -> 'b t -> 'b t =
 fun x y input pos succ await fail ->
  let succ' pos' _ = y input pos' succ await fail in
  x input pos succ' await fail

let ( <* ) : 'a t -> _ t -> 'a t =
 fun x y input pos succ await fail ->
  let succ1 pos' x' =
    let succ2 pos'' _ = succ pos'' x' in
    y input pos' succ2 await fail
  in
  x input pos succ1 await fail

let get_input : int -> Cstruct.t t =
 fun n input pos succ await _fail ->
  if pos + n <= Cstruct.length input then
    let b = Cstruct.sub input pos n in
    succ pos b
  else await pos

let char : char -> char t =
 fun ch input pos succ await fail ->
  let succ' pos' v =
    let ch' = Cstruct.get_char v 0 in
    if Char.equal ch ch' then succ (pos' + 1) ch' else fail pos' [] ""
  in
  get_input 1 input pos succ' await fail

let string : string -> string t =
 fun s input pos succ await fail ->
  let len = String.length s in
  let succ' pos' s' =
    if Cstruct.equal s' (Cstruct.of_string s) then succ (len + pos') s
    else fail pos' [] ""
  in
  get_input len input pos succ' await fail

let parse : 'a t -> input -> 'a parse =
 fun p input ->
  let ret = ref @@ `Awaiting_input 0 in
  let fail pos stack_trace e = ret := `Fail (pos, stack_trace, e) in
  let succ pos v : unit = ret := `Ok (pos, v) in
  let await pos = ret := `Awaiting_input pos in
  p input 0 succ await fail ; !ret
