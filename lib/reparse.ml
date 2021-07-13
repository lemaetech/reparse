(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

module type PARSER = sig
  type 'a t

  type 'a promise

  type input

  type pos = int

  val parse : ?pos:pos -> input -> 'a t -> ('a * pos, string) result promise

  (** {2 Monadic operators} *)

  val return : 'a -> 'a t

  val unit : unit t

  val ignore : _ t -> unit t

  val fail : string -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( <*> ) : 'a t -> ('a -> 'b) t -> 'b t

    val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t

    val ( <$$> ) : 'a t * 'b t -> ('a -> 'b -> 'c) -> 'c t

    val ( <$$$> ) : 'a t * 'b t * 'c t -> ('a -> 'b -> 'c -> 'd) -> 'd t

    val ( <$$$$> ) :
      'a t * 'b t * 'c t * 'd t -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e t

    val ( <$ ) : 'a -> 'b t -> 'a t

    val ( $> ) : 'a t -> 'b -> 'b t

    val ( *> ) : _ t -> 'b t -> 'b t

    val ( <* ) : 'a t -> _ t -> 'a t

    val ( <|> ) : 'a t -> 'a t -> 'a t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( <?> ) : 'a t -> string -> 'a t
  end

  include module type of Infix

  module Let_syntax : sig
    val return : 'a -> 'a t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    module Let_syntax : sig
      val return : 'a -> 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t

      val bind : 'a t -> f:('a -> 'b t) -> 'b t

      val both : 'a t -> 'b t -> ('a * 'b) t

      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

      val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

      val map4 :
        'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
    end
  end

  (** {2 Char/String parsers} *)

  val peek_char : char t

  val peek_char_opt : char option t

  val peek_string : int -> string t

  val any_char : char t

  val unsafe_any_char : char t

  val char : char -> char t

  val char_if : (char -> bool) -> char t

  val string_cs : string -> string t

  val string_ci : string -> string t

  val string_of_chars : char list -> string t

  val take_string : int -> string t

  val take_cstruct : int -> Cstruct.t t

  val unsafe_take_cstruct : int -> Cstruct.t t

  val unsafe_take_cstruct_ne : int -> Cstruct.t t

  (** {2 Alternate parsers} *)

  val any : ?failure_msg:string -> 'a t list -> 'a t

  val alt : 'a t -> 'a t -> 'a t

  val optional : 'a t -> 'a option t

  (** {2 Boolean} *)

  val not_ : 'a t -> unit t

  val is : 'a t -> bool t

  val is_not : 'a t -> bool t

  (** {2 Repetition} *)

  val recur : ('a t -> 'a t) -> 'a t

  val all : 'a t list -> 'a list t

  val all_unit : _ t list -> unit t

  val skip : ?at_least:int -> ?up_to:int -> _ t -> int t

  val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t

  val take_while_cb :
    ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit t) -> 'a t -> unit t

  val take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t

  val take_between : ?sep_by:_ t -> start:_ t -> end_:_ t -> 'a t -> 'a list t

  (** RFC 5234 parsers *)

  val alpha : char t

  val alpha_num : char t

  val lower_alpha : char t

  val upper_alpha : char t

  val bit : char t

  val ascii_char : char t

  val cr : char t

  val crlf : string t

  val control : char t

  val digit : char t

  val digits : string t

  val dquote : char t

  val hex_digit : char t

  val htab : char t

  val lf : char t

  val octet : char t

  val space : char t

  val vchar : char t

  val whitespace : char t

  (** {2 Input manipulation} *)

  val advance : int -> unit t

  val eoi : unit t

  val trim_input_buffer : unit t

  val pos : int t

  val last_trimmed_pos : int t

  val input_buffer_size : int option t

  val of_promise : 'a promise -> 'a t
end

module type PROMISE = sig
  type 'a t

  val return : 'a -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type INPUT = sig
  type t

  type 'a promise

  type input

  val create : input -> t

  val trim_buffer : t -> pos:int -> unit promise

  val get_char : t -> pos:int -> [ `Char of char | `Eof ] promise

  val get_char_unbuffered : t -> pos:int -> [ `Char of char | `Eof ] promise

  val get_cstruct :
    t -> pos:int -> len:int -> [ `Cstruct of Cstruct.t | `Eof ] promise

  val get_cstruct_unbuffered :
    t -> pos:int -> len:int -> [ `Cstruct of Cstruct.t | `Eof ] promise

  val last_trimmed_pos : t -> int promise

  val buffer_size : t -> int option promise
end

module Make_promise_ops (Promise : PROMISE) = struct
  include Promise

  let ( >>= ) b f = bind f b

  let ( >>| ) b f = b >>= fun x -> return (f x)
end

module Make
    (Promise : PROMISE)
    (Input : INPUT with type 'a promise = 'a Promise.t) :
  PARSER with type 'a promise = 'a Input.promise with type input = Input.t =
struct
  type input = Input.t

  type 'a promise = 'a Input.promise

  type pos = int

  type 'a t = Input.t -> pos:pos -> ('a * pos) Input.promise

  module Promise = Make_promise_ops (Promise)

  exception Parse_failure of string

  (*+++++ Monadic operators +++++*)
  let return : 'a -> 'a t = fun v _inp ~pos -> Promise.return (v, pos)

  let unit = return ()

  let fail : string -> 'a t = fun msg _inp ~pos:_ -> raise (Parse_failure msg)

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
   fun f p inp ~pos -> Promise.(p inp ~pos >>= fun (a, pos) -> f a inp ~pos)

  let map : ('a -> 'b) -> 'a t -> 'b t =
   fun f p -> bind (fun a -> return (f a)) p

  let ignore : _ t -> unit t = fun p -> map (fun _ -> ()) p

  let both : 'a t -> 'b t -> ('a * 'b) t =
   fun a b -> bind (fun a' -> map (fun b' -> (a', b')) b) a

  let apply f g = bind (fun f' -> map f' g) f

  let map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
   fun f p q -> bind (fun p' -> map (fun q' -> f p' q') q) p

  let map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t =
   fun f p q r ->
    bind (fun p' -> bind (fun q' -> map (fun r' -> f p' q' r') r) q) p

  let map4 :
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t =
   fun f p q r s ->
    bind
      (fun p' ->
        bind (fun q' -> bind (fun r' -> map (fun s' -> f p' q' r' s') s) r) q)
      p

  module Infix = struct
    let ( >>= ) p f = bind f p

    let ( >>| ) p f = map f p

    let ( <*> ) p f = apply f p

    let ( <$> ) p f = map f p

    let ( <$$> ) (a, b) f = map2 f a b

    let ( <$$$> ) (a, b, c) f = map3 f a b c

    let ( <$$$$> ) (a, b, c, d) f = map4 f a b c d

    let ( <$ ) v p = p >>| fun _ -> v

    let ( $> ) p v = p >>| fun _ -> v

    let ( *> ) : _ t -> 'b t -> 'b t = fun p q -> p >>= fun _ -> q

    let ( <* ) : 'a t -> _ t -> 'a t =
     fun p q ->
      p >>= fun a ->
      q >>| fun _ -> a

    let ( <|> ) : 'a t -> 'a t -> 'a t =
     fun p q inp ~pos ->
      Promise.catch
        (fun () -> p inp ~pos)
        (function Parse_failure _ -> q inp ~pos | e -> raise e)

    let ( let* ) = ( >>= )

    let ( and* ) = both

    let ( let+ ) = ( >>| )

    let ( and+ ) = both

    let ( <?> ) : 'a t -> string -> 'a t =
     fun p msg inp ~pos ->
      Promise.catch
        (fun () -> p inp ~pos)
        (function Parse_failure _ -> fail msg inp ~pos | e -> raise e)
  end

  include Infix

  module Let_syntax = struct
    let return = return

    let ( >>= ) p f = bind f p

    let ( >>| ) p f = map f p

    module Let_syntax = struct
      let return = return

      let map p ~f = map f p

      let bind p ~f = bind f p

      let both = both

      let map2 p q ~f = map2 f p q

      let map3 p q r ~f = map3 f p q r

      let map4 p q r s ~f = map4 f p q r s
    end
  end

  let parse ?(pos = 0) (inp : Input.t) (p : 'a t) :
      ('a * pos, string) result promise =
    Promise.(
      catch
        (fun () -> p inp ~pos >>| fun (a, pos) -> Ok (a, pos))
        (function
          | Parse_failure msg -> return (Error msg)
          | e ->
              let msg =
                Format.sprintf "%s, %s"
                  (Printexc.get_backtrace ())
                  (Printexc.to_string e)
              in
              return (Result.error msg)))

  (* Parser invariant: pos should not be less than the last_trimmed_pos. *)
  let check_last_trimmed : unit t =
   fun inp ~pos ->
    Promise.(
      Input.last_trimmed_pos inp >>= fun last_trimmed_pos' ->
      if pos < last_trimmed_pos' then
        fail
          (Format.sprintf
             "Invalid pos: %d. Parser position should not be less than \
              last_trimmed_pos:%d"
             pos last_trimmed_pos')
          inp ~pos
      else return ((), pos))

  let get_input : int -> Cstruct.t t =
   fun n inp ~pos ->
    Promise.(
      check_last_trimmed inp ~pos >>= fun ((), pos) ->
      Input.get_cstruct inp ~pos ~len:n >>= function
      | `Cstruct s when Cstruct.length s = n -> return (s, pos)
      | `Cstruct _ ->
          fail (Format.sprintf "pos:%d, n:%d not enough input" pos n) inp ~pos
      | `Eof -> fail (Format.sprintf "pos:%d, n:%d eof" pos n) inp ~pos)

  (*+++++ String/Char parsers ++++++*)

  let peek_char : char t =
   fun inp ~pos ->
    Promise.(
      check_last_trimmed inp ~pos >>= fun ((), pos) ->
      Input.get_char inp ~pos >>= function
      | `Char c -> return (c, pos)
      | `Eof -> fail (Format.sprintf "[peek_char] pos:%d eof" pos) inp ~pos)

  let peek_char_opt : char option t =
   fun inp ~pos ->
    Promise.(
      Input.get_char inp ~pos >>= function
      | `Char c -> return (Some c, pos)
      | `Eof -> return (None, pos))

  let peek_string : int -> string t = fun n -> get_input n >>| Cstruct.to_string

  let any_char : char t =
   fun inp ~pos ->
    Promise.(
      catch
        (fun () -> peek_char inp ~pos >>= fun (c, pos) -> return (c, pos + 1))
        (fun (_ : exn) ->
          fail (Format.sprintf "[any_char] pos:%d eof" pos) inp ~pos))

  let unsafe_any_char : char t =
   fun inp ~pos ->
    Promise.(
      Input.get_char_unbuffered inp ~pos >>= function
      | `Char c -> return (c, pos + 1)
      | `Eof ->
          fail (Format.sprintf "[unsafe_any_char] pos:%d eof" pos) inp ~pos)

  let char : char -> char t =
   fun c inp ~pos ->
    Promise.(
      peek_char inp ~pos >>= fun (c', pos) ->
      if c' = c then return (c, pos + 1)
      else
        fail
          (Format.sprintf "[char] pos:%d, expected %C, got %C" pos c c')
          inp ~pos)

  let char_if : (char -> bool) -> char t =
   fun f inp ~pos ->
    Promise.(
      peek_char inp ~pos >>= fun (c, pos) ->
      if f c then return (c, pos + 1)
      else fail (Format.sprintf "[char_if] pos:%d %C" pos c) inp ~pos)

  let string_ci : string -> string t =
   fun s inp ~pos ->
    Promise.(
      let len = String.length s in
      get_input len inp ~pos >>= fun (s', pos) ->
      let s' = Cstruct.to_string s' in
      if String.(equal (lowercase_ascii s) (lowercase_ascii s')) then
        return (s, pos + len)
      else fail (Format.sprintf "[string_ci] %S" s) inp ~pos)

  let string_cs : string -> string t =
   fun s inp ~pos ->
    Promise.(
      let len = String.length s in
      get_input len inp ~pos >>= fun (cstr, pos) ->
      let cstr' = Cstruct.of_string s in
      if Cstruct.equal cstr cstr' then return (s, pos + len)
      else fail (Format.sprintf "[string_cs] %S" s) inp ~pos)

  let string_of_chars : char list -> string t =
   fun chars -> return (String.of_seq @@ List.to_seq chars)

  let take_cstruct : int -> Cstruct.t t =
   fun n inp ~pos ->
    Promise.(get_input n inp ~pos >>= fun (s, pos) -> return (s, pos + n))

  let take_string : int -> string t =
   fun n -> take_cstruct n >>| fun cs -> Cstruct.to_string cs

  let unsafe_take_cstruct : int -> Cstruct.t t =
   fun n inp ~pos ->
    Promise.(
      Input.get_cstruct_unbuffered inp ~pos ~len:n >>= function
      | `Cstruct s when Cstruct.length s = n -> return (s, pos + n)
      | `Cstruct _ ->
          fail
            (Format.sprintf
               "[unsafe_take_cstruct] pos:%d, n:%d not enough input" pos n)
            inp ~pos
      | `Eof ->
          fail
            (Format.sprintf "[unsafe_take_cstruct] pos:%d, n:%d eof" pos n)
            inp ~pos)

  let unsafe_take_cstruct_ne : int -> Cstruct.t t =
   fun n inp ~pos ->
    Promise.(
      Input.get_cstruct_unbuffered inp ~pos ~len:n >>= function
      | `Cstruct s -> return (s, pos + Cstruct.length s)
      | `Eof ->
          fail
            (Format.sprintf "[unsafe_take_cstruct] pos:%d, n:%d eof" pos n)
            inp ~pos)

  (*++++++ Alternates +++++*)

  let any : ?failure_msg:string -> 'a t list -> 'a t =
   fun ?failure_msg parsers inp ~pos ->
    Promise.(
      let rec loop = function
        | [] ->
            let failure_msg =
              match failure_msg with
              | Some msg -> msg
              | None -> "[any] all parsers failed"
            in
            fail failure_msg inp ~pos
        | p :: parsers ->
            catch
              (fun () -> p inp ~pos)
              (function
                | Parse_failure _ -> (loop [@tailcall]) parsers | e -> raise e)
      in
      loop parsers)

  let alt = ( <|> )

  let optional : 'a t -> 'a option t =
   fun p inp ~pos ->
    Promise.(
      catch
        (fun () -> p inp ~pos >>= fun (a, pos) -> return (Some a, pos))
        (function Parse_failure _ -> return (None, pos) | e -> raise e))

  (*+++++ Boolean +++++*)

  let not_ : 'a t -> unit t =
   fun p inp ~pos ->
    Promise.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> `Fail)
        (function Parse_failure _ -> return `Success | e -> raise e)
      >>= function
      | `Fail -> fail "[not_] expected failure but succeeded" inp ~pos
      | `Success -> return ((), pos))

  let is : 'a t -> bool t =
   fun p inp ~pos ->
    Promise.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> (true, pos))
        (function Parse_failure _ -> return (false, pos) | e -> raise e))

  let is_not : 'a t -> bool t =
   fun p inp ~pos ->
    Promise.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> (false, pos))
        (function Parse_failure _ -> return (true, pos) | e -> raise e))

  (*+++++ Repetition +++++*)

  let recur : ('a t -> 'a t) -> 'a t =
   fun f ->
    let rec p inp ~pos = f p inp ~pos in
    p

  let all : 'a t list -> 'a list t =
   fun parsers inp ~pos ->
    let items = ref [] in
    let rec loop pos = function
      | [] -> Promise.return (List.rev !items, pos)
      | p :: parsers ->
          Promise.(
            catch
              (fun () ->
                p inp ~pos >>= fun (a, pos) ->
                items := a :: !items;
                (loop [@tailcall]) pos parsers)
              (function
                | Parse_failure err ->
                    fail
                      (Format.sprintf "[all] one of the parsers failed: %s" err)
                      inp ~pos
                | e -> raise e))
    in
    loop pos parsers

  let all_unit : _ t list -> unit t =
   fun parsers inp ~pos ->
    let rec loop pos = function
      | [] -> Promise.return ((), pos)
      | p :: parsers ->
          Promise.(
            catch
              (fun () ->
                p inp ~pos >>= fun (_, pos) -> (loop [@tailcall]) pos parsers)
              (function
                | Parse_failure err ->
                    fail
                      (Format.sprintf "[all] one of the parsers failed: %s" err)
                      inp ~pos
                | e -> raise e))
    in
    loop pos parsers

  let skip : ?at_least:int -> ?up_to:int -> 'a t -> int t =
   fun ?(at_least = 0) ?up_to p inp ~pos ->
    if at_least < 0 then invalid_arg "at_least"
    else if Option.is_some up_to && Option.get up_to < 0 then
      invalid_arg "up_to"
    else ();
    let up_to = Option.value up_to ~default:(-1) in
    let rec loop pos skipped_count =
      if up_to = -1 || skipped_count < up_to then
        Promise.(
          catch
            (fun () ->
              p inp ~pos >>= fun (_, pos) ->
              (loop [@tailcall]) pos (skipped_count + 1))
            (fun (_ : exn) -> check skipped_count pos))
      else check skipped_count pos
    and check skipped_count pos =
      if skipped_count >= at_least then Promise.return (skipped_count, pos)
      else
        fail
          (Format.sprintf "[skip] skipped_count:%d at_least:%d" skipped_count
             at_least)
          inp ~pos
    in
    loop pos 0

  let sep_by_to_bool ?sep_by =
    match sep_by with
    | None -> return true
    | Some sep_by -> (
        optional sep_by >>| function Some _ -> true | None -> false)

  let take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t =
   fun ?(at_least = 0) ?up_to ?sep_by p inp ~pos ->
    if at_least < 0 then invalid_arg "at_least"
    else if Option.is_some up_to && Option.get up_to < 0 then
      invalid_arg "up_to"
    else ();
    let sep_by = sep_by_to_bool ?sep_by in
    let items = ref [] in
    let up_to = Option.value ~default:(-1) up_to in
    let p' = (p, sep_by) <$$> fun v sep_by_ok -> (v, sep_by_ok) in
    let rec loop pos taken_count =
      if up_to = -1 || taken_count < up_to then
        Promise.(
          catch
            (fun () ->
              p' inp ~pos >>= fun ((a, sep_by_ok), pos) ->
              items := a :: !items;
              if sep_by_ok then (loop [@tailcall]) pos (taken_count + 1)
              else check taken_count pos)
            (fun (_ : exn) -> check taken_count pos))
      else check taken_count pos
    and check taken_count pos =
      if taken_count >= at_least then Promise.return (List.rev !items, pos)
      else
        fail
          (Format.sprintf "[take] taken_count:%d at_least:%d" taken_count
             at_least)
          inp ~pos
    in
    loop pos 0

  let take_while_cb :
      ?sep_by:_ t ->
      while_:bool t ->
      on_take_cb:('a -> unit t) ->
      'a t ->
      unit t =
   fun ?sep_by ~while_ ~on_take_cb p inp ~pos ->
    let sep_by = sep_by_to_bool ?sep_by in
    let p' = (p, sep_by) <$$> fun v sep_by_ok -> (v, sep_by_ok) in
    let rec loop : unit -> unit t =
     fun () inp ~pos ->
      Promise.(
        while_ inp ~pos >>= fun (continue, _pos) ->
        if continue then
          catch
            (fun () ->
              p' inp ~pos >>= fun ((v, sep_by_ok), pos) ->
              on_take_cb v inp ~pos >>= fun ((), pos) ->
              if sep_by_ok then (loop [@tailcall]) () inp ~pos
              else return ((), pos))
            (function Parse_failure _ -> return ((), pos) | e -> raise e)
        else return ((), pos))
    in
    loop () inp ~pos

  let take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t =
   fun ?sep_by ~while_ p ->
    let items = ref [] in
    take_while_cb ?sep_by ~while_
      ~on_take_cb:(fun a ->
        items := a :: !items;
        unit)
      p
    >>= fun () -> return (List.rev !items)

  let take_between : ?sep_by:_ t -> start:_ t -> end_:_ t -> 'a t -> 'a list t =
   fun ?sep_by ~start ~end_ p ->
    start >>= fun _ ->
    take_while ?sep_by ~while_:(is_not end_) p >>= fun l ->
    end_ >>| fun _ -> l

  (*+++++ RFC 5234 parsers *)

  let named_ch name f inp ~pos =
    Promise.(
      catch
        (fun () -> (char_if f) inp ~pos)
        (function
          | Parse_failure err ->
              fail (Format.sprintf "[%s] %s" name err) inp ~pos
          | e -> raise e))

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let alpha = named_ch "ALPHA" is_alpha

  let alpha_num =
    named_ch "ALPHA NUM" (function c -> is_alpha c || is_digit c)

  let lower_alpha =
    named_ch "LOWER ALPHA" (function 'a' .. 'z' -> true | _ -> false)

  let upper_alpha =
    named_ch "UPPER ALPHA" (function 'A' .. 'Z' -> true | _ -> false)

  let bit = named_ch "BIT" (function '0' | '1' -> true | _ -> false)

  let cr = named_ch "CR" (function '\r' -> true | _ -> false)

  let crlf = string_ci "\r\n" <?> "[crlf]"

  let digit = named_ch "DIGIT" is_digit

  let digits =
    take ~at_least:1 digit
    >>| (fun d -> List.to_seq d |> String.of_seq)
    <?> "[digits]"

  let dquote = named_ch "DQUOTE" (function '"' -> true | _ -> false)

  let htab = named_ch "HTAB" (function '\t' -> true | _ -> false)

  let lf = named_ch "LF" (function '\n' -> true | _ -> false)

  let octet = any_char

  let space = named_ch "SPACE" (function '\x20' -> true | _ -> false)

  let vchar =
    named_ch "VCHAR" (function '\x21' .. '\x7E' -> true | _ -> false)

  let whitespace = named_ch "WSP" (function ' ' | '\t' -> true | _ -> false)

  let ascii_char =
    named_ch "US-ASCII" (function '\x00' .. '\x7F' -> true | _ -> false)

  let control =
    named_ch "CONTROL" (function
      | '\x00' .. '\x1F' | '\x7F' -> true
      | _ -> false)

  let hex_digit =
    named_ch "HEX DIGIT" (function
      | c when is_digit c -> true
      | 'A' .. 'F' -> true
      | _ -> false)

  (*+++++ Parser State +++++*)

  let advance : int -> unit t =
   fun n inp ~pos ->
    Promise.(
      catch
        (fun () ->
          get_input n inp ~pos >>= fun (_, pos) -> return ((), pos + n))
        (function
          | Parse_failure err ->
              fail
                (Format.sprintf "[advance] pos:%d, error: %s" pos err)
                inp ~pos
          | e -> raise e))

  let eoi : unit t =
   fun inp ~pos ->
    Promise.(
      Input.get_cstruct inp ~pos ~len:1 >>= function
      | `Cstruct _ -> fail (Format.sprintf "[eoi] pos:%d, not eoi" pos) inp ~pos
      | `Eof -> return ((), pos))

  let trim_input_buffer : unit t =
   fun inp ~pos ->
    Promise.(Input.trim_buffer inp ~pos >>= fun () -> return ((), pos))

  let pos : int t = fun _inp ~pos -> Promise.return (pos, pos)

  let last_trimmed_pos : int t =
   fun inp ~pos ->
    Promise.(
      Input.last_trimmed_pos inp >>= fun last_trimmed_pos' ->
      return (last_trimmed_pos', pos))

  let of_promise : 'a promise -> 'a t =
   fun prom _inp ~pos -> Promise.(prom >>= fun a -> return (a, pos))

  let input_buffer_size : int option t =
   fun inp ~pos ->
    Promise.(Input.buffer_size inp >>| fun sz_opt -> (sz_opt, pos))
end

module type BUFFERED_INPUT = sig
  type t

  type 'a promise

  val read : t -> len:int -> [ `Cstruct of Cstruct.t | `Eof ] promise
end

module Make_buffered_input
    (Promise : PROMISE)
    (Input : BUFFERED_INPUT with type 'a promise = 'a Promise.t) :
  INPUT with type 'a promise = 'a Promise.t with type input = Input.t = struct
  type t = {
    input : Input.t;
    mutable buf : Cstruct.t;
    mutable last_trimmed_pos : int;
        (* An input position marker. The marker restricts the parser from
           backtracking beyound this point. Any attempt to do so will raise an
           exception. *)
  }

  type input = Input.t

  let create input = { input; buf = Cstruct.empty; last_trimmed_pos = 0 }

  type 'a promise = 'a Promise.t

  module Promise = Make_promise_ops (Promise)

  let trim_buffer t ~pos =
    let pos' = pos - t.last_trimmed_pos in
    let bytes_to_copy = Cstruct.length t.buf - pos' in
    let buf =
      if bytes_to_copy <= 0 then Cstruct.empty
      else Cstruct.sub t.buf pos' bytes_to_copy
    in
    t.buf <- buf;
    t.last_trimmed_pos <- pos;
    Promise.return ()

  let buffer_pos_len t ~pos ~len =
    let pos' = pos - t.last_trimmed_pos in
    let len' = Cstruct.length t.buf - (pos' + len) in
    (pos', len')

  let get_char_common t ~pos =
    let pos', len' = buffer_pos_len t ~pos ~len:1 in
    if len' >= 0 then Promise.return (`Return (Cstruct.get_char t.buf pos'))
    else
      Promise.(
        Input.read t.input ~len:1 >>| function
        | `Cstruct cs -> `Additional_byte_read (Cstruct.get_char cs 0)
        | `Eof -> `Eof)

  let get_char t ~pos =
    Promise.(
      get_char_common t ~pos >>| function
      | `Return c -> `Char c
      | `Additional_byte_read c ->
          let new_buf = Cstruct.create_unsafe (Cstruct.length t.buf + 1) in
          Cstruct.blit t.buf 0 new_buf 0 (Cstruct.length t.buf);
          Cstruct.set_char new_buf (Cstruct.length new_buf - 1) c;
          t.buf <- new_buf;
          `Char c
      | `Eof -> `Eof)

  let get_char_unbuffered t ~pos =
    Promise.(
      get_char_common t ~pos >>| function
      | `Return c -> `Char c
      | `Additional_byte_read c -> `Char c
      | `Eof -> `Eof)

  let get_cstruct_common t ~pos ~len =
    let open Promise in
    let pos', len' = buffer_pos_len t ~pos ~len in
    if len' >= 0 then return (`Return (Cstruct.sub t.buf pos' len))
    else
      let len = abs len' in
      Input.read t.input ~len >>| function
      | `Cstruct cs -> `Additional_bytes_read (cs, pos')
      | `Eof -> `Eof

  let get_cstruct_unbuffered t ~pos ~len =
    let open Promise in
    get_cstruct_common t ~pos ~len >>| function
    | `Eof -> `Eof
    | `Return cs -> `Cstruct cs
    | `Additional_bytes_read (cs, pos') ->
        if Cstruct.length cs <= len then `Cstruct cs
        else
          let b1 =
            let len' = len - Cstruct.length cs in
            Cstruct.sub t.buf pos' len'
          in
          `Cstruct Cstruct.(append b1 cs)

  let get_cstruct t ~pos ~len =
    let open Promise in
    get_cstruct_common t ~pos ~len >>| function
    | `Eof -> `Eof
    | `Return buf -> `Cstruct buf
    | `Additional_bytes_read (additional_bytes, pos') ->
        let new_buf = Cstruct.append t.buf additional_bytes in
        let len' = Cstruct.length new_buf - pos' in
        let len = if len' < len then len' else len in
        t.buf <- new_buf;
        `Cstruct (Cstruct.sub t.buf pos' len)

  let last_trimmed_pos t = Promise.return t.last_trimmed_pos

  let buffer_size t = Promise.return @@ Some (Cstruct.length t.buf)
end

module type UNBUFFERED_INPUT = sig
  type t

  type 'a promise

  val read : t -> pos:int -> len:int -> [ `Cstruct of Cstruct.t | `Eof ] promise
end

module Make_unbuffered_input
    (Promise : PROMISE)
    (Input : UNBUFFERED_INPUT with type 'a promise = 'a Promise.t) :
  INPUT with type 'a promise = 'a Promise.t with type input = Input.t = struct
  module Promise = Make_promise_ops (Promise)

  type t = { input : Input.t; mutable last_trimmed_pos : int }

  type input = Input.t

  type 'a promise = 'a Promise.t

  let create input = { input; last_trimmed_pos = 0 }

  let trim_buffer t ~pos =
    t.last_trimmed_pos <- pos;
    Promise.return ()

  let get_char t ~pos =
    Promise.(
      Input.read t.input ~pos ~len:1 >>| function
      | `Cstruct cs -> `Char (Cstruct.get_char cs 0)
      | `Eof -> `Eof)

  let get_char_unbuffered = get_char

  let get_cstruct t ~pos ~len = Input.read t.input ~pos ~len

  let get_cstruct_unbuffered = get_cstruct

  let last_trimmed_pos t = Promise.return @@ t.last_trimmed_pos

  let buffer_size _ = Promise.return None
end

module String = struct
  module Promise = struct
    type 'a t = 'a

    let return a = a

    let catch f e = try f () with exn -> e exn

    let bind f promise = f promise
  end

  module Input =
    Make_unbuffered_input
      (Promise)
      (struct
        type t = Cstruct.t

        type 'a promise = 'a

        let read t ~pos ~len =
          let len' = Cstruct.length t - pos in
          if len' <= 0 then `Eof
          else if len' > len then `Cstruct (Cstruct.sub t pos len)
          else `Cstruct (Cstruct.sub t pos len')
      end)

  include Make (Promise) (Input)

  let create_input input = Input.create input

  let create_input_from_string s = Input.create (Cstruct.of_string s)

  let create_input_from_bigstring ?off ?len ba =
    Input.create (Cstruct.of_bigarray ?off ?len ba)
end
