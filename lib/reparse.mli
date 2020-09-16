(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** {2 Types} *)

type +'a t
(** Represents a parser which can parse value ['a]. *)

exception
  Parse_error of
    { offset : int
    ; line_number : int
    ; column_number : int
    ; msg : string }
(** [Parser_error (lnum, cnum, msg)] Raised by failed parsers. [lnum], [cnum] is
    line number and column number respectively at the time of parser failure.
    [msg] contains a descriptive error message. {b Note} [lnum], [cnum] is both
    [0] if line tracking is disabled. *)

val parse : ?track_lnum:bool -> string -> 'a t -> 'a
(** [parse ~count_lines input p] executes parser [p] with [input]. If
    [track_lnum] is true then the parser tracks both line and column numbers. It
    is set to [false] by default.

    @raise Parse_error
    {[
      open Reparse
      let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in

      (* Track line, column number. *)
      let r = parse ~track_lnum:true "hello world" p in
      r = (1, 11)

      (* Don't track line, column number. *)
      let r = parse "hello world" p in
      r = (0, 0)
    ]} *)

val return : 'a -> 'a t
(** [return v] creates a new parser that always returns [v].

    {[
      open Reparse
      let r = parse "" (return 5) in
      r = 5

      let r = parse "" (return "hello") in
      r = "hello"
    ]} *)

(** {2 Operators} *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= f] Bind. Executes parser [p] which returns value [a]. If it succeeds
    then it executes [f a] which returns a new parse [q].

    {[
      open Reparse
      let p = char 'h' >>= fun c -> return (Char.code c) in
      let r = parse "hello" p in
      r = 104
    ]} *)

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
(** [p >|= f] Map. Executes parser [p] which returns value [a]. It then executes
    [f a] which returns value [c]. This is same as [p >>= (fun a -> return a)].

    {[
      open Reparse
      let p = char 'h' >|= fun c -> Char.code c in
      let r = parse "hello" p in
      r = 104
    ]} *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** [p <*> q] Applicative. Executes parsers [p] and [q] which returns function
    [f] and [a] respectively. It then applies [f a].

    {[
      open Reparse
      let c = return (fun a -> a + 2) <*> return 2 in
      let r = parse "" c in
      r = 4
    ]} *)

val ( <$ ) : 'b -> 'a t -> 'b t
(** [v <$ p] replaces the result of [p] with [v]. *)

(** Mappers over pairs of parsers. Joins their parsing results together. *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

val ( *> ) : _ t -> 'a t -> 'a t
(** [p *> q] executes parser [p] and [q]. However, the result of [p] is
    discarded. The parse value of [q] is returned instead. *)

val ( <* ) : 'a t -> _ t -> 'a t
(** [p <* q] discards result of parser [q] and returns [p] instead. *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** [p <|> q] tries both parsers. Takes the result of [p] if it succeeds.
    Otherwise returns the result of the [q]. {b Note} If you want [q] to be lazy
    evaluated then use it with [delay] combinator. *)

val ( <?> ) : 'a t -> string -> 'a t
(** [p <?> err_mg] parse [p]. If it fails then fail with error message
    [err_msg]. Used as a last choice in [<|>], e.g.
    [a <|> b <|> c <?> "expected a b c"].

    {[
      open Reparse
      let p = next <?> "[error]" in
      let r =
      try let _ = parse "" p in false
      with Parse_error {offset=0;line_number=0;column_number=0;msg="[error]"} -> true
      | _ -> false in
      r = true
    ]} *)

val named : string -> 'a t -> 'a t
(** [named name p] names parser [p] with [name]. The name is used on error
    message. This may be helpful when debugging parsers. *)

val delay : (unit -> 'a t) -> 'a t
(** [delay f] delays the computation of [p] until it is required. [p] is
    [p = f ()]. Use it together with [<|>]. *)

val advance : int -> unit t
(** [advance n] advances parser by the given [n] number of characters. *)

val is_eoi : bool t
(** [is_eoi] returns [true] if parser has reached end of input. *)

val eoi : unit t
(** [eoi] Parse the end of input to be successful. *)

val fail : string -> 'a t
(** [fail msg] fails the parser with [msg]. *)

val failing : 'a t -> unit t
(** [failing p] succeeds if and only if [p] fails to parse. *)

val lnum : int t
(** [lnum] return the current line number. *)

val cnum : int t
(** [cnum] returns the current column number. *)

val offset : int t
(** [offset] returns the current input offset. *)

val unit : unit t
(** [unit] is [return ()]. *)

(** {2 Parsers} *)

val peek_char : char t
(** [peek_char t] returns a character from input without consuming it.

    {[
      open Reparse
      let p = peek_char in
      let r = parse "hello" p in
      r = 'h'

      (* Input offset value remains the same. *)
      let p = peek_char *> offset in
      let r = parse "hello" p in
      r = 0
    ]} *)

val peek_string : int -> string t
(** [peek_string n] attempts to retrieve string of length [n] from input exactly
    and return it. No input is consumed. *)

val next : char t
(** [next] consumes and returns the next char of input. *)

val char : char -> char t
(** [char c] accepts character [c] from input exactly and returns it. Fails
    Otherwise.*)

val satisfy : (char -> bool) -> char t
(** [satisfy f] accepts a char [c] from input if [f c] is true and returns it. *)

val string : string -> unit t
(** [string s] accepts [s] exactly. *)

val skip : ?at_least:int -> ?up_to:int -> _ t -> int t
(** [skip ~at_least ~up_to p] parses [p] zero or more times while discarding the
    result. If [at_least] is given, then [p] must execute successfully
    [at_least] times to be considered successful. Default of [at_least] is 0. If
    [up_to] is given then [p] is executed maximum [up_to] times. By default
    [up_to] doesn't have an upper bound value. Returns the count of times [p]
    was skipped successfully.

    {[
      open Reparse

      ;;
      let r = parse "     " (skip space) in
      r = 5
    ]} *)

val many :
  ?at_least:int -> ?up_to:int -> ?sep_by:unit t -> 'a t -> (int * 'a list) t
(** [many ~at_least ~up_to ~sep_by p] executes [p] zero or more times up to the
    given upper bound [up_to]. If [at_least] is given, [p] is expected to
    succeed the lower bound of [at_least] times. Default of [at_least] is [0].
    Thre is no upper bound on execution of [p] is [up_to] is not given. If
    [sep_by] is given execution of [p] must be followed by successful execution
    of [sep_by]. Returns the count of times [p] was executed along with the list
    of successfully parsed values.

    {[
      open Reparse

      ;;
      let r = parse "aaaaa" (many (char 'a')) in
      r = (5, ['a'; 'a'; 'a'; 'a'; 'a'])
    ]} *)

val not_followed_by : 'a t -> 'b t -> 'a t
(** [not_followed_by a b] Succeeds if parser [p] succeeds and parser [q] fails.
    The second parser [q] never consumes any input. *)

val optional : 'a t -> 'a option t
(** [optional p] parses [p] and retruns [SOME a] if successful. Otherwise
    returns [NONE]. *)

val line : string t
(** [line] accepts and returns a line of input delimited by either [\n] or
    [\r\n]. Returns [None] if end of input is reached.

    {[
      open Reparse

      let l = parse "line1\r\nline2" line in
      l = "line1"
    ]} *)

val backtrack : 'a t -> 'a t
(** [backtrack p] executes parser [p] and returns it value. After which the
    parser state is reset to the state equal to before the execution of [p]. *)

(** {2 Core parsers - RFC 5254, Appending B.1. *)

val alpha : char t
(** [alpha] parse a character in range [A- Z] or [a-z]. *)

val alpha_num : char t
(** [alpha_num] parse a character in range [A-Z] or [a-z] or [0-9]. *)

val bit : char t
(** [bit] returns a character which is wither '0' or '1'. *)

val ascii_char : char t
(** [ascii_char] parses any US-ASCII character. *)

val cr : char t
(** [cr] parse CR '\r' character. *)

val crlf : unit t
(** [crlf] parse CRLF - \r\n - string. *)

val control : char t
(** [control] parse characters in range %x00-1F or %x7F. *)

val digit : char t
(** [digit] parse a digit character - [0 .. 9]. *)

val dquote : char t
(** [dquote] parse double quote character - '"'. *)

val hex_digit : char t
(** [hex_digit] parse a hexadecimal digit - [0..9, A, B, C, D, E, F]. *)

val htab : char t
(** [htab] parse a horizontal tab ('\t') character. *)

val lf : char t
(** [lf] parse a linefeed ('\n') character. *)

(* val lwsp : string t *)
(** [lwsp] parse linear whitespaces - *(WSP / CRLF WSP). {b Note} Use of LWSP is
    discouraged. See https://tools.ietf.org/html/rfc5234#appendix-B.1 *)

val octect : char t
(** [octect] parse a byte of character, [%x00-FF] 8 bytes of data. *)

val space : char t
(** [space] parse a space character. *)

val spaces : char list t

val vchar : char t
(** [vchar] parse a Visible (printing) character. *)

val whitespace : char t
(** [whitespace] parse a space or horizontal - ' ' or '\t' - character. *)

module Infix : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <$ ) : 'b -> 'a t -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( *> ) : _ t -> 'a t -> 'a t
  val ( <* ) : 'a t -> _ t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
end
