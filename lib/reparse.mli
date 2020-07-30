(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** Reparse is an easy to learn and use parser combinator library. It is
    designed to aid authoring recursive descent style parsers. It removes the
    tedium of having to maintain parser/lexer input buffer. It emphasises and
    enables monadic style of writing parsers. As such the parser uses [error]
    type to denote errors in parsing rather than the ocaml exception. *)

(** {2 Types} *)

type error = [ `Msg of string ]
(** Parser error type. *)

type input = [ `String of string | `Bigstring of Bigstringaf.t ]
(** Represents parser input. *)

type (+'a, +'error) t
(** The main parser type.['a] denotes the successful parse value while ['error]
    denotes error raised by the parser. *)

val sexp_of_error : error -> Sexplib0.Sexp.t

val advance : int -> (unit, [> error ]) t
(** [advance n] advances parser by the given [n] number of characters. Always
    succeeds. *)

val end_of_input : (bool, [> error ]) t
(** [end_of_input] returns [true] if parser has reached end of input. Always
    succeeds. *)

(** {2 Executing} *)

val parse : input -> ('a, ([> error ] as 'b)) t -> ('a, 'b) result
(** [parse input p] executes parser [p] with input [input]. *)

(** {2 Basic Parsers} *)

val char : char -> (char, [> error ]) t
(** [char c] accepts character [c] from input exactly and returns it. Fails
    Otherwise.*)

val char_if : (char -> bool) -> (char option, [> error ]) t
(** [char_if f] accepts and returns [Some c] if [f c] is true. Otherwise it
    returns [None]. Always succeeds. *)

val satisfy : (char -> bool) -> (char, [> error ]) t
(** [satisfy f] accepts a char [c] from input if [f c] is true and returns it.
    Otherwise it fails. *)

val peek_char : (char option, [> error ]) t
(** [peek_char t] returns a character at the current position in the parser.
    Always suceeds and returns [None] if EOF is reached. *)

val peek_char_fail : (char, [> error ]) t
(** Same as [peek_char] except the call fails if [end of input] is encountered. *)

val any_char : (char, [> error ]) t
(** [any_char] accepts any char and returns it. Fails if EOF reached. *)

val peek_string : int -> (string option, [> error ]) t
(** [peek_string n] attempts to match string of length [n] from input exactly
    and return it. If it isn't matched [None] is returned. *)

val string : string -> (string, [> error ]) t
(** [string s] accepts [s] exactly and returns it. *)

val string_if : string -> (string option, [> error ]) t
(** [string_if s] accepts and returns [Some s] if [s] matches input. Otherwise
    returns [None]. Always succeeds. *)

val skip_while : (char -> bool) -> (unit, [> error ]) t
(** [skip_while f] keeps accepting [c] if [f c] is [true]. [c] is discarded.
    Always succeeds. *)

val count_skip_while : (char -> bool) -> (int, [> error ]) t
(** [count_skip_while f] accepts characters from input while [f c] is true and
    records the count of times the input char was accepted. The accepted chars
    are discarded and the count is returned. *)

val count_skip_while_string : int -> (string -> bool) -> (int, [> error ]) t
(** [count_skip_while_string n f] accepts string [s] of length [n] if [f s] is
    true. The accepted string [s] is discarded and the count of times the string
    was accepted is noted. The count is then returned. *)

val take_while : (char -> bool) -> (string, [> error ]) t
(** [take_while f] keeps accepting character [c] from input while [f c] is true.
    It then concatenates the accepted characters and converts it into a string
    and returns it. *)

val take_while_n : int -> (char -> bool) -> (string, [> error ]) t
(** [take_while_n n f] similar in functionality to [take_while]. The parser
    however has a maximum upper bound [n] on the number of characters it
    accepts. *)

(** {2 Constructors} *)

val ok : 'a -> ('a, [> error ]) t
(** [ok v] creates a new parser that always returns the constant [v]. *)

val fail : ([> error ] as 'e) -> (_, 'e) t
(** [fail err] creates a parser that always fails with [err]. *)

(** {2 Combinators} *)

val ( <|> ) : ('a, 'error) t -> ('a, 'error) t -> ('a, 'error) t
(** [p <|> q] creates a parser that executes [p] and returns the result if it is
    successful. If false then it executes [q] and returns it. *)

val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
(** [p >>= q] executes [p] and if it succeeds executes [q] and returns it's
    result else it returns the result of executing [p]. *)

val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
(** [p >>| f] executes [p] and then if it is successful returns [a]. It then
    executes [f a] and returns the result. *)

val ( >>|? ) : ('a, 'error) t -> ('error -> 'c) -> ('a, 'c) t
(** [p >>|? f] executes [p] and maps [error] via [f error] if [p] results in
    [error]. *)

val ( >>*? ) : ('a, 'error) t -> 'f -> ('a, 'f) t
(** [p >>*? e] executes [p] and returns error [e] discarding [error] if [p]
    fails. *)

val ( *> ) : (_, 'error) t -> ('a, 'error) t -> ('a, 'error) t
(** [p *> b] is [p >>= fun _ -> b] *)

val ( <* ) : ('a, 'error) t -> (_, 'error) t -> ('a, 'error) t
(** [p <* q] discards result from [q] and returns [p] *)

val ( *>| ) : (_, 'error) t -> 'a -> ('a, 'error) t
(** [p *>| a] is [p >>| fun _ -> a] *)

val many : ('a, [> error ]) t -> ('a list, [> error ]) t
(** [many p] runs p zero or more times and returns a list of results from the
    runs of p.*)

val count_skip_many : ('a, [> error ]) t -> (int, [> error ]) t
(** [count_skip_many p] runs [p] zeor or more times *)

val line : (string option, [> error ]) t
(** [line] accepts and returns a line of input delimited by either [\n] or
    [\r\n]. Returns [None] if end of input is reached. *)
