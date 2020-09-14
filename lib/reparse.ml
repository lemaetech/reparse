(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)
type state =
  { src : string
  ; offset : int
  ; track_lnum : bool
  ; lnum : int (* line count. *)
  ; cnum : int (* column count. *) }

type 'a t = state -> state * 'a

exception Parse_error of int * int * string

let parse ?(track_lnum = false) src p =
  let lnum, cnum = if track_lnum then (1, 0) else (0, 0) in
  let state = {src; offset = 0; track_lnum; lnum; cnum} in
  try
    let (_ : state), a = p state in
    Ok a
  with exn -> Error exn

let advance n state =
  let len = String.length state.src in
  if state.offset + n < len then
    let offset = state.offset + n in
    let state =
      if state.track_lnum then (
        let lnum = ref state.lnum in
        let cnum = ref state.cnum in
        for i = state.offset to offset do
          let c = state.src.[i] in
          if Char.equal c '\n' then (
            lnum := !lnum + 1 ;
            cnum := 0 )
          else cnum := !cnum + 1
        done ;
        {state with offset; lnum = !lnum; cnum = !cnum} )
      else {state with offset}
    in
    (state, ())
  else raise @@ Parse_error (state.lnum, state.cnum, "advance failed - EOF.")

let return v state = (state, v)
let fail msg state = raise @@ Parse_error (state.lnum, state.cnum, msg)

let ( >>= ) p f state =
  let state, a = p state in
  f a state

let ( >|= ) p f = p >>= fun a -> return (f a)
let ( <*> ) p q = p >>= fun f -> q >|= f
let ( <$> ) f p = return f <*> p
let map2 f p q = return f <*> p <*> q
let map3 f p q r = return f <*> p <*> q <*> r
let map4 f p q r s = return f <*> p <*> q <*> r <*> s
let ( <$ ) v p = (fun _ -> v) <$> p
let ( *> ) p q = p >>= fun _ -> q
let ( <* ) p q = p >>= fun a -> q *> return a
let ( <|> ) p q state = try p state with (_ : exn) -> q state
let ( <?> ) p err_msg state = try p state with (_ : exn) -> fail err_msg state
let delay f state = f () state

let named name p state =
  try p state
  with exn ->
    fail
      (Format.sprintf "%s failed with error %s" name (Printexc.to_string exn))
      state

let peek_char state =
  match state.src.[state.offset] with
  | c -> (state, c)
  | exception Invalid_argument _ -> fail "EOF" state

let peek_string len state =
  if state.offset + len < String.length state.src then
    (state, String.sub state.src state.offset len)
  else fail "peek_string failed - EOF." state

let next = peek_char <* advance 1

let is_eoi state =
  let is_eof =
    match peek_char state with
    | _, _                -> false
    | exception (_ : exn) -> true
  in
  (state, is_eof)

let eoi =
  is_eoi
  >|= function
  | true  -> ()
  | false -> failwith "not EOF"

let failing p state =
  let succeed =
    try
      let _, _ = p state in
      false
    with _ -> true
  in
  if succeed then (state, ()) else fail "parsing failing failed" state

let lnum state = (state, state.lnum)
let cnum state = (state, state.cnum)

let char c =
  next
  >>= fun c2 ->
  if Char.equal c c2 then c <$ advance 1
  else fail @@ Format.sprintf "char '%c' expected instead of %c" c c2

let satisfy f =
  next
  >>= fun c2 ->
  if f c2 then c2 <$ advance 1
  else fail @@ Format.sprintf "satisfy failed on char '%c'" c2

let string s =
  let len = String.length s in
  peek_string len
  <?> "parsing string '%s' failed with EOF"
  >>= fun s2 ->
  if String.equal s s2 then advance len
  else fail @@ Format.sprintf "unable to parse \"%s\"" s

let skip ?(at_least = 0) ?up_to p =
  if at_least < 0 then invalid_arg "at_least"
  else if Option.is_some up_to && Option.get up_to < 0 then invalid_arg "up_to"
  else () ;

  let up_to = Option.value up_to ~default:(-1) in
  let rec loop count =
    if up_to = -1 || count < up_to then
      try p *> loop (count + 1) with (_ : exn) -> return count
    else return count
  in
  loop 0

let many ?(at_least = 0) ?up_to ?(sep_by = return ()) p =
  if at_least < 0 then invalid_arg "at_least"
  else if Option.is_some up_to && Option.get up_to < 0 then invalid_arg "up_to"
  else () ;

  let upto = Option.value up_to ~default:(-1) in
  let rec loop count acc =
    if upto = -1 || count < upto then
      try p <* sep_by >>= fun a -> loop (count + 1) (a :: acc)
      with (_ : exn) -> return (count, acc)
    else return (count, acc)
  in
  loop 0 []
  >|= fun (count, acc) ->
  if count >= at_least then (count, List.rev acc)
  else
    failwith
      (Format.sprintf
         "parser many didn't execute successfully at least %d times"
         at_least)

let not_followed_by p q = p <* failing q
let optional p = try Option.some <$> p with _ -> return None

let backtrack p state =
  let _, a = p state in
  (state, a)

let line state =
  let rec loop buf state =
    let _, (c1, c2) =
      (map2 (fun c1 c2 -> (c1, c2)) (optional next) (optional next)) state
    in
    match (c1, c2) with
    | Some '\r', Some '\n' -> (Buffer.contents buf <$ advance 2) state
    | Some '\n', _         -> (Buffer.contents buf <$ advance 1) state
    | Some c1, _           ->
        Buffer.add_char buf c1 ;
        let state, () = advance 1 state in
        loop buf state
    | None, _              -> fail "parsing line failed, EOF reached." state
  in
  loop (Buffer.create 1) state

let char_parser name p state =
  try p state
  with exn ->
    fail
      (Format.sprintf "parsing '%s' failed - %s" name (Printexc.to_string exn))
      state

let is_alpha = function
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
      true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _          -> false

let alpha = char_parser "ALPHA" (satisfy is_alpha)

let alpha_num =
  char_parser "ALPHA NUM" (satisfy (function c -> is_alpha c || is_digit c))

let bit =
  char_parser
    "BIT"
    (satisfy (function
        | '0'
        | '1' ->
            true
        | _ -> false))

let ascii_char =
  char_parser
    "US-ASCII"
    (satisfy (function
        | '\x00' .. '\x7F' -> true
        | _                -> false))

let cr =
  char_parser
    "CR"
    (satisfy (function
        | '\r' -> true
        | _    -> false))

let crlf = try string "\r\n" with _ -> fail "unable to parse CRLF"

let control =
  char_parser
    "CONTROL"
    (satisfy (function
        | '\x00' .. '\x1F'
        | '\x7F' ->
            true
        | _ -> false))

let digit = char_parser "DIGIT" (satisfy is_digit)

let dquote =
  char_parser
    "DQUOTE"
    (satisfy (function
        | '"' -> true
        | _   -> false))

let hex_digit =
  char_parser
    "HEX DIGIT"
    (satisfy (function
        | c when is_digit c -> true
        | 'A' .. 'F' -> true
        | _ -> false))

let htab =
  char_parser
    "HTAB"
    (satisfy (function
        | '\t' -> true
        | _    -> false))

let lf =
  char_parser
    "LF"
    (satisfy (function
        | '\n' -> true
        | _    -> false))

let octect = next <* advance 1

let space =
  char_parser
    "SPACE"
    (satisfy (function
        | '\x20' -> true
        | _      -> false))

let spaces = snd <$> many space

let vchar =
  char_parser
    "VCHAR"
    (satisfy (function
        | '\x21' .. '\x7E' -> true
        | _                -> false))

let whitespace =
  char_parser
    "VCHAR"
    (satisfy (function
        | '\x20'
        | '\x09' ->
            true
        | _ -> false))
