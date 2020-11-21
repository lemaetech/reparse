(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

class type input =
  object
    method eof : int -> bool

    method sub : offset:int -> len:int -> string

    method nth : int -> char
  end

class string_input s =
  object
    method eof i = i >= String.length s

    method sub ~offset ~len = String.sub s offset len

    method nth i =
      match s.[i] with
      | c -> c
      | exception Invalid_argument _ -> raise End_of_file
  end

type state =
  { input : input
  ; track_lnum : bool (* Track line numbers. *)
  ; mutable offset : int (* Input offset. *)
  ; mutable lnum : int (* Line count. *)
  ; mutable cnum : int (* Column count. *)
  }

type 'a t = state -> ok:('a -> unit) -> err:(exn -> unit) -> unit

exception
  Parser of
    { offset : int
    ; line_number : int
    ; column_number : int
    ; msg : string
    }

let error ~err msg state =
  err
    (Parser
       { offset = state.offset
       ; line_number = state.lnum
       ; column_number = state.cnum
       ; msg
       })
;;

let parse ?(track_lnum = false) p input =
  let lnum, cnum = if track_lnum then 1, 1 else 0, 0 in
  let state = { input; offset = 0; track_lnum; lnum; cnum } in
  let value = ref None in
  p state ~ok:(fun a -> value := Some a) ~err:(fun e -> raise e);
  match !value with
  | None -> assert false
  | Some a -> a
;;

let parse_string ?(track_lnum = false) p s =
  let input = new string_input s in
  parse ~track_lnum p input
;;

let fail : string -> 'a t = fun err_msg state ~ok:_ ~err -> error ~err err_msg state

let next state ~ok ~err =
  match state.input#nth state.offset with
  | c ->
    state.offset <- state.offset + 1;
    if state.track_lnum
    then
      if Char.equal c '\n'
      then (
        state.lnum <- state.lnum + 1;
        state.cnum <- 1)
      else state.cnum <- state.cnum + 1;
    ok c
  | exception _ -> error ~err "[next]" state
;;

let pure v _state ~ok ~err:_ = ok v
let unit = pure ()
let pos state = state.offset, state.lnum, state.cnum

let backtrack state (o, l, c) =
  assert (0 <= o && o <= state.offset);
  state.offset <- o;
  state.lnum <- l;
  state.cnum <- c
;;

module Infix = struct
  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
   fun p f state ~ok ~err -> p state ~ok:(fun a -> f a state ~ok ~err) ~err
 ;;

  let ( >|= ) : 'a t -> ('a -> 'b) -> 'b t =
   fun p f st ~ok ~err -> p st ~ok:(fun a -> ok (f a)) ~err
 ;;

  let ( <*> ) p q = p >>= fun f -> q >|= f
  let ( <$> ) f p = pure f <*> p
  let ( <$ ) v p = (fun _ -> v) <$> p
  let ( *> ) p q = p >>= fun _ -> q
  let ( <* ) p q = p >>= fun a -> a <$ q

  let ( <|> ) : 'a t -> 'a t -> 'a t =
   fun p q state ~ok ~err ->
    let init_pos = pos state in
    p state ~ok ~err:(fun _e ->
        backtrack state init_pos;
        q state ~ok ~err)
 ;;

  let ( <?> ) : 'a t -> string -> 'a t =
   fun p err_msg state ~ok ~err ->
    let offset = state.offset in
    p state ~ok ~err:(fun e ->
        if state.offset = offset then error ~err err_msg state else err e)
 ;;

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >|= )
end

open Infix

let alt = ( <|> )
let bind = ( >>= )
let map = ( <$> )
let map2 f p q = pure f <*> p <*> q
let map3 f p q r = pure f <*> p <*> q <*> r
let map4 f p q r s = pure f <*> p <*> q <*> r <*> s

let any : 'a t list -> 'a t =
 fun parsers state ~ok ~err ->
  let item = ref None in
  let err' () = error ~err "[any] all parsers failed" state in
  let rec loop = function
    | [] -> err' ()
    | p :: tl ->
      let init_pos = pos state in
      p
        state
        ~ok:(fun a -> item := Some a)
        ~err:(fun _ ->
          backtrack state init_pos;
          (loop [@tailrec]) tl)
  in
  loop parsers;
  match !item with
  | Some a -> ok a
  | None -> err' ()
;;

let all : 'a t list -> 'a list t =
 fun parsers state ~ok ~err ->
  let items = ref [] in
  let init_pos = pos state in
  let rec loop = function
    | [] -> ok (List.rev !items)
    | p :: tl ->
      p
        state
        ~ok:(fun a ->
          items := a :: !items;
          (loop [@tailrec]) tl)
        ~err:(fun _ ->
          backtrack state init_pos;
          error ~err "[all] one of the parsers failed" state)
  in
  loop parsers
;;

let all_unit : 'a t list -> unit t =
 fun parsers state ~ok ~err ->
  let parsers = List.map (fun p -> p *> unit) parsers in
  ((all parsers <?> "[all_unit] one of the parsers failed") *> unit) state ~ok ~err
;;

let delay p state ~ok ~err = Lazy.force p state ~ok ~err

let named name p state ~ok ~err =
  p state ~ok ~err:(fun e ->
      error ~err (Format.sprintf "[%s] %s" name (Printexc.to_string e)) state)
;;

let peek_char : char t =
 fun state ~ok ~err ->
  match state.input#nth state.offset with
  | c -> ok c
  | exception _ -> error ~err "[peek_char]" state
;;

let peek_string len state ~ok ~err =
  match state.input#sub ~offset:state.offset ~len with
  | s -> ok s
  | exception _e -> error ~err "[peek_string]" state
;;

let is_done state = state.input#eof state.offset
let is_eoi state ~ok ~err:_ = ok (is_done state)

let eoi : unit t =
 fun state ~ok ~err ->
  if is_done state then ok () else error ~err "[eoi] expected EOI" state
;;

let not_ : 'a t -> unit t =
 fun p state ~ok ~err ->
  let error' () = error ~err "[failing] expected failure to succeed" state in
  p state ~ok:(fun _ -> error' ()) ~err:(fun _ -> ok ())
;;

let is_not : 'a t -> bool t =
 fun p state ~ok ~err:_ ->
  let init_pos = pos state in
  p
    state
    ~ok:(fun _ ->
      backtrack state init_pos;
      ok false)
    ~err:(fun _ ->
      backtrack state init_pos;
      ok true)
;;

let is : 'a t -> bool t =
 fun p state ~ok ~err:_ ->
  let init_pos = pos state in
  p
    state
    ~ok:(fun _ ->
      backtrack state init_pos;
      ok true)
    ~err:(fun _ ->
      backtrack state init_pos;
      ok false)
;;

let lnum state ~ok ~err:_ = ok state.lnum
let cnum state ~ok ~err:_ = ok state.cnum
let offset state ~ok ~err:_ = ok state.offset

let char : char -> char t =
 fun c state ~ok ~err ->
  peek_char
    state
    ~ok:(fun c2 ->
      if Char.equal c c2
      then (c <$ next) state ~ok ~err
      else error ~err (Format.sprintf "[char] expected '%c'" c) state)
    ~err
;;

let char_if : (char -> bool) -> char t =
 fun f state ~ok ~err ->
  peek_char
    state
    ~ok:(fun c2 ->
      if f c2 then (c2 <$ next) state ~ok ~err else error ~err "[char_if]" state)
    ~err
;;

let not_followed_by p q = p <* not_ q

let optional : 'a t -> 'a option t =
 fun p state ~ok ~err:_ -> p state ~ok:(fun a -> ok (Some a)) ~err:(fun _ -> ok None)
;;

let recur f =
  let rec p st ~ok ~err = f p st ~ok ~err in
  p
;;

let skip : ?at_least:int -> ?up_to:int -> 'a t -> int t =
 fun ?(at_least = 0) ?up_to p state ~ok ~err ->
  if at_least < 0
  then invalid_arg "at_least"
  else if Option.is_some up_to && Option.get up_to < 0
  then invalid_arg "up_to"
  else ();
  let init_pos = pos state in
  let up_to = ref (Option.value up_to ~default:(-1)) in
  let skip_count = ref 0 in
  let rec loop offset count =
    if !up_to = -1 || count < !up_to
    then (
      let init_pos = pos state in
      p
        state
        ~ok:(fun _ ->
          if offset <> state.offset
          then (loop [@tailcall]) state.offset (count + 1)
          else skip_count := count)
        ~err:(fun _ ->
          backtrack state init_pos;
          skip_count := count))
    else skip_count := count
  in
  loop state.offset 0;
  if !skip_count >= at_least
  then ok !skip_count
  else (
    backtrack state init_pos;
    error ~err (Format.sprintf "[skip] unable to parse at_least %d times" at_least) state)
;;

let string : string -> string t =
 fun s state ~ok ~err ->
  let len = String.length s in
  peek_string
    len
    state
    ~ok:(fun s2 ->
      if String.equal s s2
      then (s <$ skip ~up_to:len next) state ~ok ~err
      else error ~err ("[string] " ^ s) state)
    ~err
;;

let skip_while : _ t -> while_:bool t -> int t =
 fun p ~while_ state ~ok ~err:_ ->
  let condition = ref true in
  let skip_count = ref 0 in
  let do_condition () =
    let init_pos = pos state in
    while_
      state
      ~ok:(fun condition' -> condition := condition')
      ~err:(fun _ -> condition := false);
    backtrack state init_pos
  in
  do_condition ();
  while !condition do
    let init_pos = pos state in
    (p *> unit)
      state
      ~ok:(fun _ ->
        skip_count := !skip_count + 1;
        do_condition ())
      ~err:(fun _ ->
        backtrack state init_pos;
        condition := false)
  done;
  ok !skip_count
;;

let take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t =
 fun ?(at_least = 0) ?up_to ?sep_by p state ~ok ~err ->
  if at_least < 0
  then invalid_arg "at_least"
  else if Option.is_some up_to && Option.get up_to < 0
  then invalid_arg "up_to"
  else ();
  let sep_by =
    match sep_by with
    | None -> pure true
    | Some p ->
      optional p
      >|= (function
      | Some _ -> true
      | None -> false)
  in
  let upto = Option.value up_to ~default:(-1) in
  let take_count = ref 0 in
  let values = ref [] in
  let ok2 (count', values') =
    take_count := count';
    values := values'
  in
  let rec loop count offset acc =
    if upto = -1 || count < upto
    then (
      let init_pos = pos state in
      let p = map2 (fun v continue -> v, continue) p sep_by in
      p
        state
        ~ok:(fun (a, continue) ->
          if offset <> state.offset && continue
          then (loop [@tailcall]) (count + 1) state.offset (a :: acc)
          else if offset <> state.offset && not continue
          then ok2 (count + 1, a :: acc)
          else ok2 (count, acc))
        ~err:(fun _ ->
          backtrack state init_pos;
          ok2 (count, acc)))
    else ok2 (count, acc)
  in
  let init_pos = pos state in
  loop 0 state.offset [];
  if !take_count >= at_least
  then ok (List.rev !values)
  else (
    backtrack state init_pos;
    error ~err (Format.sprintf "[take] unable to parse at least %d times" at_least) state)
;;

let take_while_cb
    : ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> int t
  =
 fun ?sep_by ~while_ ~on_take_cb p state ~ok ~err:_ ->
  let cond = ref true in
  let take_count = ref 0 in
  let eval_while () =
    let init_pos = pos state in
    while_ state ~ok:(fun cond' -> cond := cond') ~err:(fun _ -> cond := false);
    backtrack state init_pos
  in
  let sep_by =
    match sep_by with
    | None -> pure true
    | Some p ->
      optional p
      >|= (function
      | Some _ -> true
      | None -> false)
  in
  eval_while ();
  while !cond do
    let init_pos = pos state in
    let p = map2 (fun v continue -> v, continue) p sep_by in
    p
      state
      ~ok:(fun (a, continue) ->
        take_count := !take_count + 1;
        on_take_cb a;
        if continue then eval_while () else cond := false)
      ~err:(fun _ ->
        backtrack state init_pos;
        cond := false)
  done;
  ok !take_count
;;

let take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t =
 fun ?sep_by ~while_ p state ~ok ~err ->
  let items = ref [] in
  let take_count = ref 0 in
  let on_take_cb a = items := a :: !items in
  let sep_by =
    match sep_by with
    | None -> unit
    | Some p -> p *> unit
  in
  take_while_cb
    p
    ~sep_by
    ~while_
    ~on_take_cb
    state
    ~ok:(fun count -> take_count := count)
    ~err;
  ok (List.rev !items)
;;

let named_ch name f state ~ok ~err =
  (char_if f) state ~ok ~err:(fun exn ->
      error ~err (Format.sprintf "[%s] %s" name (Printexc.to_string exn)) state)
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let alpha = named_ch "ALPHA" is_alpha
let alpha_num = named_ch "ALPHA NUM" (function c -> is_alpha c || is_digit c)

let bit =
  named_ch "BIT" (function
      | '0' | '1' -> true
      | _ -> false)
;;

let cr =
  named_ch "CR" (function
      | '\r' -> true
      | _ -> false)
;;

let crlf = string "\r\n" <?> "[crlf]"
let digit = named_ch "DIGIT" is_digit
let digits = take ~at_least:1 digit >|= fun d -> List.to_seq d |> String.of_seq

let dquote =
  named_ch "DQUOTE" (function
      | '"' -> true
      | _ -> false)
;;

let htab =
  named_ch "HTAB" (function
      | '\t' -> true
      | _ -> false)
;;

let lf =
  named_ch "LF" (function
      | '\n' -> true
      | _ -> false)
;;

let octet = next

let space =
  named_ch "SPACE" (function
      | '\x20' -> true
      | _ -> false)
;;

let spaces = take ~at_least:1 space

let vchar =
  named_ch "VCHAR" (function
      | '\x21' .. '\x7E' -> true
      | _ -> false)
;;

let whitespace =
  named_ch "WSP" (function
      | ' ' | '\t' -> true
      | _ -> false)
;;

let ascii_char =
  named_ch "US-ASCII" (function
      | '\x00' .. '\x7F' -> true
      | _ -> false)
;;

let control =
  named_ch "CONTROL" (function
      | '\x00' .. '\x1F' | '\x7F' -> true
      | _ -> false)
;;

let hex_digit =
  named_ch "HEX DIGIT" (function
      | c when is_digit c -> true
      | 'A' .. 'F' -> true
      | _ -> false)
;;

let line : [ `LF | `CRLF ] -> string t =
 fun line_delimiter state ~ok ~err ->
  let line_delimiter =
    match line_delimiter with
    | `LF -> lf *> unit
    | `CRLF -> crlf *> unit
  in
  let buf = Buffer.create 0 in
  take_while_cb
    next
    ~while_:(is_not line_delimiter)
    ~on_take_cb:(fun c -> Buffer.add_char buf c)
    state
    ~ok:(fun (_ : int) -> ())
    ~err;
  (is_eoi
  >>= function
  | true -> unit
  | false -> line_delimiter)
    state
    ~ok:(fun (_ : unit) -> ())
    ~err;
  ok (Buffer.contents buf)
;;
