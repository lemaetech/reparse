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

  val parse : 'a t -> input -> ('a, string) result promise

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

  val char : char -> char t

  val char_if : (char -> bool) -> char t

  val string_cs : string -> string t

  val string_ci : string -> string t

  val string_of_chars : char list -> string t

  val take_string : int -> string t

  val take_cstruct : int -> Cstruct.t t

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
    ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> unit t

  val take_while_cbt :
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

module type INPUT = sig
  type t

  type 'a promise

  val catch : (unit -> 'a promise) -> (exn -> 'a promise) -> 'a promise

  val return : 'a -> 'a promise

  val bind : ('a -> 'b promise) -> 'a promise -> 'b promise

  val trim_buffer : t -> pos:int -> unit promise

  val get_char : t -> pos:int -> [ `Char of char | `Eof ] promise

  val get_cstruct :
    t -> pos:int -> len:int -> [ `Cstruct of Cstruct.t | `Eof ] promise

  val last_trimmed_pos : t -> int promise

  val buffer_size : t -> int option promise
end

module Make (Input : INPUT) :
  PARSER with type 'a promise = 'a Input.promise with type input = Input.t =
struct
  type input = Input.t

  type 'a promise = 'a Input.promise

  type pos = int

  type 'a t = Input.t -> pos:pos -> ('a * pos) Input.promise

  module Input = struct
    include Input

    let ( >>= ) b f = bind f b

    let ( >>| ) b f = b >>= fun x -> return (f x)
  end

  exception Parse_failure of string

  (*+++++ Monadic operators +++++*)
  let return : 'a -> 'a t = fun v _inp ~pos -> Input.return (v, pos)

  let unit = return ()

  let fail : string -> 'a t = fun msg _inp ~pos:_ -> raise (Parse_failure msg)

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
   fun f p inp ~pos -> Input.(p inp ~pos >>= fun (a, pos) -> f a inp ~pos)

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
     fun p q -> p >>= fun a -> q >>| fun _ -> a

    let ( <|> ) : 'a t -> 'a t -> 'a t =
     fun p q inp ~pos ->
      Input.catch (fun () -> p inp ~pos) (fun (_ : exn) -> q inp ~pos)

    let ( let* ) = ( >>= )

    let ( and* ) = both

    let ( let+ ) = ( >>| )

    let ( and+ ) = both

    let ( <?> ) : 'a t -> string -> 'a t =
     fun p msg inp ~pos ->
      Input.catch (fun () -> p inp ~pos) (fun (_ : exn) -> fail msg inp ~pos)
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

  let parse (p : 'a t) (inp : Input.t) : ('a, string) result promise =
    Input.(
      catch
        (fun () -> p inp ~pos:0 >>| fun (a, _) -> Ok a)
        (function
          | Parse_failure msg -> return (Error msg)
          | e -> return (Printexc.to_string e |> Result.error)))

  (* Parser invariant: pos should not be less than the last_trimmed_pos. *)
  let check_last_trimmed : unit t =
   fun inp ~pos ->
    Input.(
      last_trimmed_pos inp
      >>= fun last_trimmed_pos' ->
      if pos < last_trimmed_pos' then
        fail
          (Format.sprintf
             "Invalid pos: %d. Parser position should not be less than \
              last_trimmed_pos:%d"
             pos last_trimmed_pos')
          inp ~pos
      else
        return ((), pos))

  let get_input : int -> Cstruct.t t =
   fun n inp ~pos ->
    Input.(
      check_last_trimmed inp ~pos
      >>= fun ((), pos) ->
      get_cstruct inp ~pos ~len:n
      >>= function
      | `Cstruct s when Cstruct.length s = n -> return (s, pos)
      | `Cstruct _ ->
        fail (Format.sprintf "pos:%d, n:%d not enough input" pos n) inp ~pos
      | `Eof -> fail (Format.sprintf "pos:%d, n:%d eof" pos n) inp ~pos)

  (*+++++ String/Char parsers ++++++*)

  let peek_char : char t =
   fun inp ~pos ->
    Input.(
      check_last_trimmed inp ~pos
      >>= fun ((), pos) ->
      get_char inp ~pos
      >>= function
      | `Char c -> return (c, pos)
      | `Eof -> fail (Format.sprintf "[peek_char] pos:%d eof" pos) inp ~pos)

  let peek_char_opt : char option t =
   fun inp ~pos ->
    Input.(
      get_char inp ~pos
      >>= function
      | `Char c -> return (Some c, pos)
      | `Eof -> return (None, pos))

  let peek_string : int -> string t = fun n -> get_input n >>| Cstruct.to_string

  let any_char : char t =
   fun inp ~pos ->
    Input.(
      catch
        (fun () -> peek_char inp ~pos >>= fun (c, pos) -> return (c, pos + 1))
        (fun (_ : exn) ->
          fail (Format.sprintf "[any_char] pos:%d eof" pos) inp ~pos))

  let char : char -> char t =
   fun c inp ~pos ->
    Input.(
      peek_char inp ~pos
      >>= fun (c', pos) ->
      if c' = c then
        return (c, pos + 1)
      else
        fail
          (Format.sprintf "[char] pos:%d, expected %C, got %C" pos c c')
          inp ~pos)

  let char_if : (char -> bool) -> char t =
   fun f inp ~pos ->
    Input.(
      peek_char inp ~pos
      >>= fun (c, pos) ->
      if f c then
        return (c, pos + 1)
      else
        fail (Format.sprintf "[char_if] pos:%d %C" pos c) inp ~pos)

  let string_ci : string -> string t =
   fun s inp ~pos ->
    Input.(
      let len = String.length s in
      get_input len inp ~pos
      >>= fun (s', pos) ->
      let s' = Cstruct.to_string s' in
      if String.(equal (lowercase_ascii s) (lowercase_ascii s')) then
        return (s, pos + len)
      else
        fail (Format.sprintf "[string_ci] %S" s) inp ~pos)

  let string_cs : string -> string t =
   fun s inp ~pos ->
    Input.(
      let len = String.length s in
      get_input len inp ~pos
      >>= fun (cstr, pos) ->
      let cstr' = Cstruct.of_string s in
      if Cstruct.equal cstr cstr' then
        return (s, pos + len)
      else
        fail (Format.sprintf "[string_cs] %S" s) inp ~pos)

  let string_of_chars : char list -> string t =
   fun chars -> return (String.of_seq @@ List.to_seq chars)

  let take_cstruct : int -> Cstruct.t t =
   fun n inp ~pos ->
    Input.(get_input n inp ~pos >>= fun (s, pos) -> return (s, pos + n))

  let take_string : int -> string t =
   fun n -> take_cstruct n >>| fun cs -> Cstruct.to_string cs

  (*++++++ Alternates +++++*)

  let any : ?failure_msg:string -> 'a t list -> 'a t =
   fun ?failure_msg parsers inp ~pos ->
    Input.(
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
            (fun (_ : exn) -> (loop [@tailcall]) parsers)
      in
      loop parsers)

  let alt = ( <|> )

  let optional : 'a t -> 'a option t =
   fun p inp ~pos ->
    Input.(
      catch
        (fun () -> p inp ~pos >>= fun (a, pos) -> return (Some a, pos))
        (fun (_ : exn) -> return (None, pos)))

  (*+++++ Boolean +++++*)

  let not_ : 'a t -> unit t =
   fun p inp ~pos ->
    Input.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> `Fail)
        (fun (_ : exn) -> return `Success)
      >>= function
      | `Fail -> fail "[not_] expected failure but succeeded" inp ~pos
      | `Success -> return ((), pos))

  let is : 'a t -> bool t =
   fun p inp ~pos ->
    Input.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> (true, pos))
        (fun (_ : exn) -> return (false, pos)))

  let is_not : 'a t -> bool t =
   fun p inp ~pos ->
    Input.(
      catch
        (fun () -> p inp ~pos >>| fun _ -> (false, pos))
        (fun (_ : exn) -> return (true, pos)))

  (*+++++ Repetition +++++*)

  let recur : ('a t -> 'a t) -> 'a t =
   fun f ->
    let rec p inp ~pos = f p inp ~pos in
    p

  let all : 'a t list -> 'a list t =
   fun parsers inp ~pos ->
    let items = ref [] in
    let rec loop pos = function
      | [] -> Input.return (List.rev !items, pos)
      | p :: parsers ->
        Input.(
          catch
            (fun () ->
              p inp ~pos
              >>= fun (a, pos) ->
              items := a :: !items;
              (loop [@tailcall]) pos parsers)
            (fun (e : exn) ->
              fail
                (Format.sprintf "[all] one of the parsers failed: %s"
                   (Printexc.to_string e))
                inp ~pos))
    in
    loop pos parsers

  let all_unit : _ t list -> unit t =
   fun parsers inp ~pos ->
    let rec loop pos = function
      | [] -> Input.return ((), pos)
      | p :: parsers ->
        Input.(
          catch
            (fun () ->
              p inp ~pos >>= fun (_, pos) -> (loop [@tailcall]) pos parsers)
            (fun (e : exn) ->
              fail
                (Format.sprintf "[all] one of the parsers failed: %s"
                   (Printexc.to_string e))
                inp ~pos))
    in
    loop pos parsers

  let skip : ?at_least:int -> ?up_to:int -> 'a t -> int t =
   fun ?(at_least = 0) ?up_to p inp ~pos ->
    if at_least < 0 then
      invalid_arg "at_least"
    else if Option.is_some up_to && Option.get up_to < 0 then
      invalid_arg "up_to"
    else
      ();
    let up_to = Option.value up_to ~default:(-1) in
    let rec loop pos skipped_count =
      if up_to = -1 || skipped_count < up_to then
        Input.(
          catch
            (fun () ->
              p inp ~pos
              >>= fun (_, pos) -> (loop [@tailcall]) pos (skipped_count + 1))
            (fun (_ : exn) -> check skipped_count pos))
      else
        check skipped_count pos
    and check skipped_count pos =
      if skipped_count >= at_least then
        Input.return (skipped_count, pos)
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
      optional sep_by
      >>| function
      | Some _ -> true
      | None -> false)

  let take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t =
   fun ?(at_least = 0) ?up_to ?sep_by p inp ~pos ->
    if at_least < 0 then
      invalid_arg "at_least"
    else if Option.is_some up_to && Option.get up_to < 0 then
      invalid_arg "up_to"
    else
      ();
    let sep_by = sep_by_to_bool ?sep_by in
    let items = ref [] in
    let up_to = Option.value ~default:(-1) up_to in
    let p' = (p, sep_by) <$$> fun v sep_by_ok -> (v, sep_by_ok) in
    let rec loop pos taken_count =
      if up_to = -1 || taken_count < up_to then
        Input.(
          catch
            (fun () ->
              p' inp ~pos
              >>= fun ((a, sep_by_ok), pos) ->
              items := a :: !items;
              if sep_by_ok then
                (loop [@tailcall]) pos (taken_count + 1)
              else
                check taken_count pos)
            (fun (_ : exn) -> check taken_count pos))
      else
        check taken_count pos
    and check taken_count pos =
      if taken_count >= at_least then
        Input.return (List.rev !items, pos)
      else
        fail
          (Format.sprintf "[take] taken_count:%d at_least:%d" taken_count
             at_least)
          inp ~pos
    in
    loop pos 0

  let take_while_cb :
      ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> unit t
      =
   fun ?sep_by ~while_ ~on_take_cb p inp ~pos ->
    let sep_by = sep_by_to_bool ?sep_by in
    let p' = (p, sep_by) <$$> fun v sep_by_ok -> (v, sep_by_ok) in
    let rec loop pos =
      Input.(
        while_ inp ~pos
        >>= fun (continue, _pos) ->
        if continue then
          catch
            (fun () ->
              p' inp ~pos
              >>= fun ((v, sep_by_ok), pos) ->
              on_take_cb v;
              if sep_by_ok then
                (loop [@tailcall]) pos
              else
                return ((), pos))
            (function
              | Parse_failure _ -> return ((), pos)
              | e -> raise e)
        else
          return ((), pos))
    in
    loop pos

  let take_while_cbt :
         ?sep_by:_ t
      -> while_:bool t
      -> on_take_cb:('a -> unit t)
      -> 'a t
      -> unit t =
   fun ?sep_by ~while_ ~on_take_cb p inp ~pos ->
    let sep_by = sep_by_to_bool ?sep_by in
    let p' = (p, sep_by) <$$> fun v sep_by_ok -> (v, sep_by_ok) in
    let rec loop : unit -> unit t =
     fun () inp ~pos ->
      Input.(
        while_ inp ~pos
        >>= fun (continue, _pos) ->
        if continue then
          catch
            (fun () ->
              p' inp ~pos
              >>= fun ((v, sep_by_ok), pos) ->
              on_take_cb v inp ~pos
              >>= fun ((), pos) ->
              if sep_by_ok then
                (loop [@tailcall]) () inp ~pos
              else
                return ((), pos))
            (function
              | Parse_failure _ -> return ((), pos)
              | e -> raise e)
        else
          return ((), pos))
    in
    loop () inp ~pos

  let take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t =
   fun ?sep_by ~while_ p ->
    let items = ref [] in
    take_while_cb ?sep_by ~while_ ~on_take_cb:(fun a -> items := a :: !items) p
    *> return (List.rev !items)

  let take_between : ?sep_by:_ t -> start:_ t -> end_:_ t -> 'a t -> 'a list t =
   fun ?sep_by ~start ~end_ p ->
    start *> take_while ?sep_by ~while_:(is_not end_) p <* end_

  (*+++++ RFC 5234 parsers *)

  let named_ch name f inp ~pos =
    Input.(
      catch
        (fun () -> (char_if f) inp ~pos)
        (fun (e : exn) ->
          fail (Format.sprintf "[%s] %s" name (Printexc.to_string e)) inp ~pos))

  let is_alpha = function
    | 'a' .. 'z'
    | 'A' .. 'Z' ->
      true
    | _ -> false

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let alpha = named_ch "ALPHA" is_alpha

  let alpha_num =
    named_ch "ALPHA NUM" (function c -> is_alpha c || is_digit c)

  let lower_alpha =
    named_ch "LOWER ALPHA" (function
      | 'a' .. 'z' -> true
      | _ -> false)

  let upper_alpha =
    named_ch "UPPER ALPHA" (function
      | 'A' .. 'Z' -> true
      | _ -> false)

  let bit =
    named_ch "BIT" (function
      | '0'
      | '1' ->
        true
      | _ -> false)

  let cr =
    named_ch "CR" (function
      | '\r' -> true
      | _ -> false)

  let crlf = string_ci "\r\n" <?> "[crlf]"

  let digit = named_ch "DIGIT" is_digit

  let digits =
    take ~at_least:1 digit
    >>| (fun d -> List.to_seq d |> String.of_seq)
    <?> "[digits]"

  let dquote =
    named_ch "DQUOTE" (function
      | '"' -> true
      | _ -> false)

  let htab =
    named_ch "HTAB" (function
      | '\t' -> true
      | _ -> false)

  let lf =
    named_ch "LF" (function
      | '\n' -> true
      | _ -> false)

  let octet = any_char

  let space =
    named_ch "SPACE" (function
      | '\x20' -> true
      | _ -> false)

  let vchar =
    named_ch "VCHAR" (function
      | '\x21' .. '\x7E' -> true
      | _ -> false)

  let whitespace =
    named_ch "WSP" (function
      | ' '
      | '\t' ->
        true
      | _ -> false)

  let ascii_char =
    named_ch "US-ASCII" (function
      | '\x00' .. '\x7F' -> true
      | _ -> false)

  let control =
    named_ch "CONTROL" (function
      | '\x00' .. '\x1F'
      | '\x7F' ->
        true
      | _ -> false)

  let hex_digit =
    named_ch "HEX DIGIT" (function
      | c when is_digit c -> true
      | 'A' .. 'F' -> true
      | _ -> false)

  (*+++++ Parser State +++++*)

  let advance : int -> unit t =
   fun n inp ~pos ->
    Input.(
      catch
        (fun () ->
          get_input n inp ~pos >>= fun (_, pos) -> return ((), pos + n))
        (fun (e : exn) ->
          fail
            (Format.sprintf "[advance] pos:%d, error: %s" pos
               (Printexc.to_string e))
            inp ~pos))

  let eoi : unit t =
   fun inp ~pos ->
    Input.(
      get_cstruct inp ~pos ~len:1
      >>= function
      | `Cstruct _ -> fail (Format.sprintf "[eoi] pos:%d, not eoi" pos) inp ~pos
      | `Eof -> return ((), pos))

  let trim_input_buffer : unit t =
   fun inp ~pos -> Input.(trim_buffer inp ~pos >>= fun () -> return ((), pos))

  let pos : int t = fun _inp ~pos -> Input.return (pos, pos)

  let last_trimmed_pos : int t =
   fun inp ~pos ->
    Input.(
      last_trimmed_pos inp
      >>= fun last_trimmed_pos' -> return (last_trimmed_pos', pos))

  let of_promise : 'a promise -> 'a t =
   fun prom _inp ~pos -> Input.(prom >>= fun a -> return (a, pos))

  let input_buffer_size : int option t =
   fun inp ~pos -> Input.(buffer_size inp >>| fun sz_opt -> (sz_opt, pos))
end

module String = struct
  type t' =
    { input : Cstruct.t
    ; mutable last_trimmed_pos : int
    }

  include Make (struct
    type 'a promise = 'a

    type t = t'

    let return a = a

    let bind f promise = f promise

    let catch f e =
      try f () with
      | exn -> e exn

    let trim_buffer t ~pos =
      t.last_trimmed_pos <- pos;
      return ()

    let get_char t ~pos =
      if pos + 1 <= Cstruct.length t.input then
        `Char (Cstruct.get_char t.input pos)
      else
        `Eof

    let get_cstruct t ~pos ~len =
      let len' = Cstruct.length t.input - pos in
      if len' <= 0 then
        `Eof
      else if len' > len then
        `Cstruct (Cstruct.sub t.input pos len)
      else
        `Cstruct (Cstruct.sub t.input pos len')

    let last_trimmed_pos t = return t.last_trimmed_pos

    let buffer_size _ = return None
  end)

  let input_of_string s = { input = Cstruct.of_string s; last_trimmed_pos = 0 }

  let input_of_bigstring ?off ?len ba =
    { input = Cstruct.of_bigarray ?off ?len ba; last_trimmed_pos = 0 }

  let input_of_cstruct input = { input; last_trimmed_pos = 0 }
end
