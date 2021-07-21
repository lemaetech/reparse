type 'a t = input -> pos -> 'a success -> 'a await -> failure -> unit

and input = Cstruct.t

and pos = int

and 'a parse =
  [ `Ok of pos * 'a
  | `Fail of pos * string list * string
  | `Awaiting_input of pos * 'a continue ]

and 'a continue = Cstruct.t -> unit

and failure = pos -> string list -> string -> unit

and 'a success = input -> pos -> 'a -> unit

and 'a await = pos -> 'a continue -> unit

let parse : 'a t -> input -> 'a parse =
 fun p input ->
  let ret = ref @@ `Fail (0, [], "") in
  let fail pos stack_trace e = ret := `Fail (pos, stack_trace, e) in
  let succ _input pos v : unit = ret := `Ok (pos, v) in
  let await pos continue = ret := `Awaiting_input (pos, continue) in
  p input 0 succ await fail ; !ret

let return v : 'a t = fun input pos succ _await _fail -> succ input pos v

let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
 fun p f input pos succ await fail ->
  let succ' input' pos' v = (f v) input' pos' succ await fail in
  p input pos succ' await fail

let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
 fun p f input pos succ await fail ->
  let succ' input' pos' v = succ input' pos' (f v) in
  p input pos succ' await fail

let ( *> ) : _ t -> 'b t -> 'b t =
 fun x y input pos succ await fail ->
  let succ' input' pos' _ = y input' pos' succ await fail in
  x input pos succ' await fail

let ( <* ) : 'a t -> _ t -> 'a t =
 fun x y input pos succ await fail ->
  let succ1 input' pos' x' =
    let succ2 input'' pos'' _ = succ input'' pos'' x' in
    y input' pos' succ2 await fail
  in
  x input pos succ1 await fail

let get_input : int -> unit t =
 fun n input pos succ await _fail ->
  if pos + n <= Cstruct.length input then succ input pos ()
  else
    let continue input = succ input pos () in
    await pos continue

let char : char -> char t =
 fun ch input pos succ await fail ->
  let succ' input' pos' () =
    let ch' = Cstruct.get_char input' 0 in
    if Char.equal ch ch' then succ input' (pos' + 1) ch'
    else fail pos' [] (Format.sprintf "[char] %C" ch)
  in
  get_input 1 input pos succ' await fail

let string : string -> string t =
 fun s input pos succ await fail ->
  let len = String.length s in
  let succ' input' pos' () =
    let s' = Cstruct.sub input' pos len in
    if Cstruct.equal s' (Cstruct.of_string s) then succ input' (len + pos') s
    else fail pos' [] (Format.sprintf "[string] %s" s)
  in
  get_input len input pos succ' await fail
