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
(** Represents a parser type that returns value ['a]. *)

exception Parse_error of int * int * string
(** [Parser_error (lnum, cnum, msg)] Raised by failed parsers. [lnum], [cnum] is
    line number and column number respectively at the time of parser failure.
    [msg] contains a descriptive error message. *)

val parse : ?count_lines:bool -> string -> 'a t -> ('a, exn) result
(** [parse ~count_lines input p] executes parser [p] with [input]. If
    [count_lines] is true then the parser counts line and column numbers. It is
    [false] by default. *)

val return : 'a -> 'a t
(** [return v] creates a new parser that always returns constant [v]. *)

(** {2 Operators} *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= q] executes [p] and if it succeeds executes [q] and returns it's
    result else it returns the result of executing [p]. *)

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
(** [p >|= f] executes [p] and then if it is successful returns [a]. It then
    executes [f a] and returns the result. *)

val ( *> ) : _ t -> 'a t -> 'a t
(** [p *> b] is [p >>= fun _ -> b] *)

val ( <* ) : 'a t -> _ t -> 'a t
(** [p <* q] discards result from [q] and returns [p] *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** [p <|> q] creates a parser that executes [p] and returns the result if it is
    successful. If false then it executes [q] and returns it. See [delay]. *)

val delay : (unit -> 'a t) -> 'a t
(** [delay f] delays the computation of [p] until it is required. [p] is
    [p = f ()]. Use it together with [<|>]. *)

val advance : int -> unit t
(** [advance n] advances parser by the given [n] number of characters. *)

val end_of_input : bool t
(** [end_of_input] returns [true] if parser has reached end of input. *)

val fail : string -> 'a t
(** [fail msg] fails the parser with [msg]. *)

val lnum : int t
(** [lnum] return the current line number. *)

val cnum : int t
(** [cnum] returns the current column number. *)

(** {2 Parsers} *)

val char : char -> char t
(** [char c] accepts character [c] from input exactly and returns it. Fails
    Otherwise.*)

val char_if : (char -> bool) -> char t
(** [char_if f] accepts and returns [c] if [f c] is true. *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] accepts a char [c] from input if [f c] is true and returns it.
    Otherwise it fails. *)

val peek_char : char option t
(** [peek_char t] returns a character at the current position in the parser.
    Always suceeds and returns [None] if EOF is reached. *)

val peek_string : int -> string option t
(** [peek_string n] attempts to match string of length [n] from input exactly
    and return it. If it isn't matched [None] is returned. *)

val string : string -> unit t
(** [string s] accepts [s] exactly. *)

val skip_while : (char -> bool) -> unit t
(** [skip_while f] keeps accepting [c] if [f c] is [true]. [c] is discarded.
    Always succeeds. *)

val count_skip_while : (char -> bool) -> int t
(** [count_skip_while f] accepts characters from input while [f c] is true and
    records the count of times the input char was accepted. The accepted chars
    are discarded and the count is returned. *)

val count_skip_while_string : int -> (string -> bool) -> int t
(** [count_skip_while_string n f] accepts string [s] of length [n] if [f s] is
    true. The accepted string [s] is discarded and the count of times the string
    was accepted is noted. The count is then returned. *)

val take_while : (char -> bool) -> string t
(** [take_while f] keeps accepting character [c] from input while [f c] is true.
    It then concatenates the accepted characters and converts it into a string
    and returns it. *)

val take_while_n : int -> (char -> bool) -> string t
(** [take_while_n n f] similar in functionality to [take_while]. The parser
    however has a maximum upper bound [n] on the number of characters it
    accepts. *)

val many : 'a t -> 'a list t
(** [many p] runs p zero or more times and returns a list of results from the
    runs of p.*)

val count_skip_many : 'a t -> int t
(** [count_skip_many p] runs [p] zeor or more times *)

val line : string option t
(** [line] accepts and returns a line of input delimited by either [\n] or
    [\r\n]. Returns [None] if end of input is reached. *)

(** {2 Core parsers - RFC 5254, Appending B.1. *)

val alpha : char t
(** [alpha] parse a character which is in range [A - Z] or [a .. z]. *)

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
