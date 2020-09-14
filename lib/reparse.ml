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
  ; cnum : int (* column count. *)
  ; cc : current_char }

and current_char =
  [ `Char of char
  | `Eof ]

type 'a t = state -> state * 'a

exception Parse_error of int * int * string

let pp_current_char fmt = function
  | `Char c -> Format.fprintf fmt "%c" c
  | `Eof    -> Format.fprintf fmt "EOF"

let parser_error state fmt =
  Format.kasprintf
    (fun s -> raise @@ Parse_error (state.lnum, state.cnum, s))
    fmt

let substring len state =
  if state.offset + len < String.length state.src then
    String.sub state.src state.offset len |> Option.some
  else None

let parse ?(track_lnum = false) src p =
  let lnum, cnum = if track_lnum then (1, 0) else (0, 0) in
  let state = {src; offset = 0; track_lnum; lnum; cnum; cc = `Eof} in
  try
    let (_ : state), a = p state in
    Ok a
  with exn -> Error exn

let return v state = (state, v)

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
        { state with
          offset
        ; lnum = !lnum
        ; cnum = !cnum
        ; cc = `Char state.src.[offset] } )
      else {state with offset; cc = `Char state.src.[offset]}
    in
    (state, ())
  else
    let state = {state with offset = len; cc = `Eof} in
    (state, ())

let ( >>= ) p f state =
  let state, a = p state in
  f a state

let ( >|= ) p f = p >>= fun a -> return (f a)
let ( <*> ) p q = p >>= fun f -> q >|= f
let ( <$> ) f p = return f <*> p
let ( <$$> ) f p q = return f <*> p <*> q
let ( <$$$> ) f p q r = return f <*> p <*> q <*> r
let ( <$$$$> ) f p q r s = return f <*> p <*> q <*> r <*> s
let ( <$ ) v p = (fun _ -> v) <$> p
let ( *> ) p q = p >>= fun _ -> q
let ( <* ) p q = q >>= fun _ -> p
let ( <|> ) p q state = try p state with (_ : exn) -> q state

let ( <?> ) p err_msg state =
  try p state with (_ : exn) -> parser_error state "%s" err_msg

let named name p state =
  try p state
  with exn ->
    parser_error state "%s failed with error %s" name (Printexc.to_string exn)

let delay f state = f () state

let is_eoi state =
  let is_eof =
    match state.cc with
    | `Char _ -> false
    | `Eof    -> true
  in
  (state, is_eof)

let eoi =
  is_eoi
  >|= function
  | true  -> ()
  | false -> failwith "not EOF"

let fail msg state = raise @@ Parse_error (state.lnum, state.cnum, msg)

let failing p state =
  let succeed =
    try
      let _, _ = p state in
      false
    with _ -> true
  in
  if succeed then (state, ()) else parser_error state "[failing]"

let lnum state = (state, state.lnum)
let cnum state = (state, state.cnum)

let next state =
  match state.cc with
  | `Char c -> (state, c)
  | `Eof    -> parser_error state "EOF"

let char c state =
  if state.cc = `Char c then (c <$ advance 1) state
  else
    parser_error
      state
      "char '%c' expected instead of '%a'"
      c
      pp_current_char
      state.cc

let satisfy f state =
  match state.cc with
  | `Char c when f c -> (c <$ advance 1) state
  | `Char _
  | `Eof ->
      parser_error
        state
        "satisfy is 'false' for char '%a'"
        pp_current_char
        state.cc

let peek_char state =
  let v =
    match state.cc with
    | `Char c -> Some c
    | `Eof    -> None
  in
  (state, v)

let peek_string n state = (state, substring n state)

let string s state =
  let len = String.length s in
  match substring len state with
  | Some s2 ->
      if String.equal s s2 then advance len state
      else parser_error state "%d: string \"%s\" not found" state.offset s
  | None    ->
      parser_error
        state
        "%d: got EOF while parsing string \"%s\""
        state.offset
        s

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

let many ?(at_least = 0) ?up_to p =
  if at_least < 0 then invalid_arg "at_least"
  else if Option.is_some up_to && Option.get up_to < 0 then invalid_arg "up_to"
  else () ;

  let upto = Option.value up_to ~default:(-1) in
  let rec loop count acc =
    if upto = -1 || count < upto then
      try p >>= fun a -> loop (count + 1) (a :: acc)
      with (_ : exn) -> return (count, acc)
    else return (count, acc)
  in
  loop 0 []
  >|= fun (count, acc) ->
  if count >= at_least then (count, List.rev acc)
  else
    failwith
      (Format.sprintf
         "parser didn't execute successfully at least %d times"
         at_least)

let line state =
  let peek_2chars state =
    let c1 = state.cc in
    let c2 =
      let state2, () = advance 1 state in
      state2.cc
    in
    (c1, c2)
  in
  let rec loop buf state =
    match peek_2chars state with
    | `Char '\r', `Char '\n' ->
        (Option.some @@ Buffer.contents buf <$ advance 2) state
    | `Char '\n', _          ->
        (Option.some @@ Buffer.contents buf <$ advance 1) state
    | `Char c1, _            ->
        Buffer.add_char buf c1 ;
        let state, () = advance 1 state in
        loop buf state
    | `Eof, _                -> (state, None)
  in
  loop (Buffer.create 1) state

let char_parser name p state =
  try p state
  with _ ->
    parser_error
      state
      "current char '%a' is not a '%s' character."
      pp_current_char
      state.cc
      name

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

let crlf state =
  try string "\r\n" state with _ -> parser_error state "unable to parse CRLF"

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

let octect state =
  match state.cc with
  | `Char c -> (state, c)
  | `Eof    -> parser_error state "EOF reached."

let space =
  char_parser
    "SPACE"
    (satisfy (function
        | '\x20' -> true
        | _      -> false))

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
