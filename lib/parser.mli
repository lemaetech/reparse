(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

type 'a t
(** Represents a parser which can parse value ['a]. *)

(** Represents parser input type. *)
class type input =
  object
    method eof : int -> bool
    (** [i#eof offset] returns [true] if [offset] position in [i] represents the
        end of input. *)

    method sub : offset:int -> len:int -> string
    (** [i#sub t ~offset ~len] reads and returns a string of length [len] at
        position [offset] from input [i]. May return a string of length less
        than [len]. *)

    method nth : int -> char
    (** [i#nth n] returns the [n]th char from input [i].

        @raise End_of_file if [n] is at eof. *)
  end

class string_input : string -> input
(** Represents string input parser type. *)

class file_input : Unix.file_descr -> input
(** Represents a unix file_descriptor parser input type. *)

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

val parse : ?track_lnum:bool -> input -> 'a t -> 'a
(** [parse ~tract_lnum input p] executes parser [p] with [input]. If
    [track_lnum] is true then the parser tracks both line and column numbers. It
    is set to [false] by default.

    @raise Parse_error
    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(take next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum) in
      let input = new P.string_input "hello world" in
      let r1 = P.parse ~track_lnum:true input p in
      let r2 = P.parse input p in
      r1 = (1, 12) && r2 = (0, 0)
    ]} *)

val return : 'a -> 'a t
(** [return v] returns a parser that always parses value [v].

    {[
      module P = Reparse.Parser
      open P.Infix

      let input = new P.string_input ""

      ;;
      let r = P.(parse input (P.return 5)) in
      r = 5

      ;;
      let r = P.parse input (P.return "hello") in
      r = "hello"
    ]} *)

val fail : string -> 'a t
(** [fail err_msg] creates a parser that always fails with [err_msg]. *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= f] Binder. Returns a parser as a result of applying [f a] where [a]
    is the parsed value of [p].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(char 'h' >>= fun c -> return (Char.code c)) in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = 104
    ]} *)

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
(** [p >|= f] Mapper parser. Returns a parser which encapsulates value [b] as a
    result of applying [f a] where [a] is the parsed value of [p].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(char 'h' >|= fun c -> Char.code c) in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = 104
    ]} *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** [pf <*> q] Applicative parser. Returns a parser encapsulating value [b] as a
    result of applying [f a], where [f] is the function value parsed by parser
    [pf] and [a] is the value parsed by [q].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(return (fun a -> a + 2) <*> return 2) in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = 4
    ]} *)

val ( <$ ) : 'b -> 'a t -> 'b t
(** [v <$ p] replaces the result of [p] with [v].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.("hello" <$ char 'h') in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = "hello"
    ]} *)

(** Mappers over pairs of parsers. Joins their parsing results together. *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> p] returns a parser encapsulating value [b] as a result of applying
    [f a]. [a] is value parsed by [p].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.((fun a -> a ^ " world") <$> string "hello") in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = "hello world"
    ]} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f p] is [f <$> p] *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f p q] returns a parser which encapsulates value [c] a result of
    applying [f a b]. [a, b] are the parsed value of parsers [p] and [q]
    respectively.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(map2 (fun a b -> a + b) (return 1) (return 2)) in
      let input = new P.string_input "" in
      let r = P.parse input p in
      r = 3
    ]} *)

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** [map3 f p q r] returns a parser encapsulating value [d] as a result of
    applying [f a b c]. [a, b, c] are the parsed value of parsers [p], [q] and
    [r] respectively.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p =
        P.(map3 (fun a b c -> a + b + c) (return 1) (return 2) (return 3))
      in
      let input = new P.string_input "" in
      let r = P.parse input p in
      r = 6
    ]} *)

val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
(** [map4 f p q r s] returns a parser encapsulating value [e] as a result of
    applying [f a b c d]. [a, b, c, d] are the parsed value of parsers [p], [q],
    [r] and [s] respectively.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p =
        P.(
          map4
            (fun a b c d -> a + b + c + d)
            (return 1)
            (return 2)
            (return 3)
            (return 4))
      in
      let input = new P.string_input "" in
      let r = P.parse input p in
      r = 10
    ]} *)

val ( *> ) : _ t -> 'a t -> 'a t
(** [p *> q] returns a parser which executes parsers [p] and then [q] and
    encapsulates value [a]. [a] is the parsed value of [q]. The parsed value of
    [p] is discarded.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(string "world" *> P.return "hello") in
      let input = new P.string_input "world" in
      let r = P.parse input p in
      r = "hello"
    ]} *)

val ( <* ) : 'a t -> _ t -> 'a t
(** [p <* q] similar to [*>]. However, the result of [q] is discarded instead.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(string "world" <* P.return "hello") in
      let input = new P.string_input "world" in
      let r = P.parse input p in
      r = "world"
    ]} *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** [p <|> q] Alternate parser. Returns a parser which evaluates both [p] and
    [q] returning [a] and [b] respectively. If [p] succeeds then it returns a
    parser encapsulating [a]. If [p] fails and [q] is a success, then it returns
    a parser encapsulating [b]. If both - [p] and [q] - fails, then the parser
    fails with [Parser_error]

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(char 'h' <|> char 'w') in
      let input = new P.string_input "world" in
      let r = P.parse input p in
      r = 'w'

      ;;
      let p = P.(char 'h' <|> char 'w') in
      let input = new P.string_input "hello" in
      let r = P.parse input p in
      r = 'h'

      ;;
      let p = P.(char 'h' <|> char 'w') in
      let input = new P.string_input "" in
      let r =
        try
          let _ = P.parse input p in
          false
        with _ -> true
      in
      r = true
    ]} *)

val ( <?> ) : 'a t -> string -> 'a t
(** [p <?> err_mg] If parser [p] is unable to parse successfully then fails with
    error message [err_msg]. Used as a last choice in [<|>], e.g.
    [a <|> b <|> c <?> "expected a b c"].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "" in
      let p = P.next <?> "[error]" in
      let r =
        try
          let _ = P.parse input p in
          false
        with
        | P.Parse_error
            {offset = 0; line_number = 0; column_number = 0; msg = "[error]"} ->
            true
        | _ -> false
      in
      r = true
    ]} *)

val any : 'a t list -> 'a t
(** [any l] returns a parser encapsulating value [a]. [a] is the parser value of
    the first successfully evaluated parser specified in list [l]. Specified
    parsers in [l] are evaluated sequentially from left to right. The parser
    fails if none of the parsers in [l] are evaluated successfully.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let input = new P.string_input "abc" in
      let r = P.parse input p in
      r = 'a'

      ;;
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let input = new P.string_input "yyy" in
      let r =
        try
          let _ = P.parse input p in
          false
        with _ -> true
      in
      r = true
    ]} *)

val all : 'a t list -> 'a list t
(** [all l] returns a parser encapsulating a list of of parser values
    accumulated by evaluating parsers specified in [l] sequentially - from left
    to right. It only succeeds if and only if all of the parsers in [l] succeed.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(all [char 'a'; char 'b'; char 'c']) in
      let input = new P.string_input "abc" in
      let r = P.parse input p in
      r = ['a'; 'b'; 'c']

      ;;
      let p = P.(all [char 'a'; char 'b'; char 'c']) in
      let input = new P.string_input "abd" in
      let r =
        try
          let _ = P.parse input p in
          false
        with _ -> true
      in
      r = true
    ]} *)

val all_unit : 'a t list -> unit t
(** [all_unit l] returns a parser which behaves similar to {!val:all} - except
    all of the parser values are discarded.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
      let input = new P.string_input "abc" in
      let r = P.parse input p in
      r = ()

      ;;
      let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
      let input = new P.string_input "abd" in
      let r =
        try
          let _ = P.parse input p in
          false
        with _ -> true
      in
      r = true
    ]} *)

val named : string -> 'a t -> 'a t
(** [named name p] names parser [p] with [name]. The name is used on error
    message. This may be helpful when debugging parsers. *)

val delay : 'a t Lazy.t -> 'a t
(** [delay p] returns a parser which lazyily evaluates parser [p].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(delay (lazy (char 'z')) <|> delay (lazy (char 'a'))) in
      let input = new P.string_input "abc" in
      let r = P.parse input p in
      r = 'a'
    ]} *)

val is_eoi : bool t
(** [is_eoi] returns [true] if parser has reached end of input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "" in
      let r = P.(parse input is_eoi) in
      r = true

      ;;
      let input = new P.string_input "a" in
      let r = P.(parse input is_eoi) in
      r = false
    ]} *)

val eoi : unit t
(** [eoi] returns a parser which parses end of input. Fails if parser is not at
    end of input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "" in
      let r = P.(parse input eoi) in
      r = ()

      ;;
      let input = new P.string_input "a" in
      let r =
        try
          let _ = P.(parse input eoi) in
          false
        with _ -> true
      in
      r = true
    ]} *)

val not_ : 'a t -> unit t
(** [not_ p] returns a parser which succeeds if and only if [p] fails to parse.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bbb" in
      let p = P.(not_ (char 'a')) in
      let r = P.parse input p in
      r = ()
    ]} *)

val is_not : 'a t -> bool t
(** [is_not p] returns a parser encapsulating value [true] if [p] fails to parse
    and [false] otherwise. {b Note} evaluating [p] doesn't consume any input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bbb" in
      let r = P.(parse input (is_not (char 'a'))) in
      r = true
    ]} *)

val is : 'a t -> bool t
(** [is p] returns a parser which encapsulates [true] is [p] parses
    successfully, [false] otherwise. {b Note} evaluation of [p] doesn't consume
    any input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bcb" in
      let r = P.(parse input (is (char 'b'))) in
      r = true
    ]} *)

val lnum : int t
(** [lnum] returns a parser encapsulating the current line number. The first
    line number is [1].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bcb" in
      let p = P.(next *> lnum) in
      let r = P.parse ~track_lnum:true input p in
      r = 1
    ]} *)

val cnum : int t
(** [cnum] returns the current column number. The first column number is [1].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bcb" in
      let p = P.(next *> cnum) in
      let r = P.parse ~track_lnum:true input p in
      r = 2
    ]} *)

val offset : int t
(** [offset] returns a parser encapsulating the current input offset. The first
    offset is [0].

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "bcb" in
      let p = P.(next *> offset) in
      let r = P.parse ~track_lnum:true input p in
      r = 1
    ]} *)

val unit : unit t
(** [unit] is [return ()]. *)

(** {2 Parsers} *)

val peek_char : char t
(** [peek_char t] returns a parser encapsulating a character from input without
    consuming it.

    {[
      module P = Reparse.Parser
      open P.Infix
      ;;

      let input = new P.string_input "hello" in
      let p = P.peek_char in
      let r = P.parse input p in
      r = 'h'

      let input = new P.string_input "hello" in
      (* Input offset value remains the same. *)
      let p = P.(peek_char *> offset) in
      let r = P.parse input p in
      r = 0
    ]} *)

val peek_string : int -> string t
(** [peek_string n] returns a parser encapsulating a string of length [n] from
    input. No input is consumed.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "hello" in
      let r = P.(parse input (peek_string 5)) in
      r = "hello"

      ;;
      let input = new P.string_input "hello" in
      let r = P.(parse input (peek_string 5 *> offset)) in
      r = 0
    ]} *)

val next : char t
(** [next] Returns a parser which consumes and encapsulates the next character
    of input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "hello" in
      let r = P.(parse input next) in
      r = 'h'
    ]} *)

val char : char -> char t
(** [char c] returns a parser which accepts a character [c] from input exactly.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "hello" in
      let p = P.char 'h' in
      let r = P.parse input p in
      r = 'h'
    ]} *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] returns a parser which accepts a character [c] from input if
    [f c] is true and encapsulates it.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "abc" in
      let p =
        P.satisfy (function
            | 'a' -> true
            | _   -> false)
      in
      let r = P.parse input p in
      r = 'a'
    ]} *)

val string : string -> string t
(** [string s] returns a parser which accepts [s] exactly and encapsulates it.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "hello world" in
      let p = P.string "hello" in
      let r = P.parse input p in
      r = "hello"
    ]} *)

val skip : ?at_least:int -> ?up_to:int -> _ t -> int t
(** [skip ~at_least ~up_to p] returns a parser which discards values returned by
    evaluating parser [p] repeatedly.

    The lower and upper bound of repetition is specified by arguments [at_least]
    and [up_to] respectively. The default value of [at_least] is 0. The default
    value of [up_to] is unspecified, i.e. there is no upper limit.

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [up_to] upper bound value is reached

    The parser encapsulates the count of times [p] was evaluated successfully.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "     " in
      let r = P.(parse input (skip space)) in
      r = 5
    ]} *)

val skip_while : _ t -> while_:bool t -> int t
(** [skip_while p ~while_] returns a parser which discards parsed values
    returned by evaluating parser [p] repeatedly.

    [p] is only evaluated when [while_] evaluates to [true].

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [while_] returns [false]

    {b Note} [while_] does not consume input.

    The parser encapsulates the count of times [p] was evaluated successfully.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "     " in
      let p = P.(skip_while next ~while_:(is space)) in
      let r = P.parse input p in
      r = 5
    ]} *)

val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t
(** [take ~at_least ~up_to ~sep_by p] returns a parser which encapsulates a list
    of values returned by evaluating parser [p] repeatedly.

    The lower and upper bound of repetition is specified by arguments [at_least]
    and [up_to] respectively. The default value of [at_least] is [0]. The
    default value of [up_to] is unspecified, i.e. there is no upper limit.

    If [sep_by] is specified then the evaluation of [p] must be followed by a
    successful evaluation of [sep_by]. The parsed value of [sep_by] is
    discarded.

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [sep_by] evaluates to failure
    - [up_to] upper boudn value is reached

    The parser fails if the count of repetition of [p] does not match the value
    specified by [at_least].

    {[
      module P = Reparse.Parser
      open P.Infix

      let input = new P.string_input "aaaaa" in
      let p = P.(take (char 'a')) in
      let r = P.parse input p in
      r = ['a'; 'a'; 'a'; 'a';'a']
      ;;


      let input = new P.string_input "a,a,a,a,a" in
      (* specify sep_by restriction. *)
      let p = P.(take ~sep_by:(char ',') (char 'a')) in
      let r = P.parse input p in
      r = ['a'; 'a'; 'a'; 'a']
      ;;

      let input = new P.string_input "a,a,a,a,a" in
      (* lower bound restriction met *)
      let p = P.(take ~at_least:3 ~sep_by:(char ',') (char 'a')) in
      let r = P.parse input p in
      r = ['a'; 'a'; 'a'; 'a']

      ;;
      let input = new P.string_input "a,a,a,a,a" in
      let p = P.(take ~at_least:5 ~sep_by:(char ',') (char 'a')) in
      let r =
        try
          let _ = P.parse input p in
          false
        with _ -> true
        (* lower bound not met *)
      in
      r = true

      ;;
      let input = new P.string_input "a,a,a,a,a" in
      (* upper bound restriction *)
      let p = P.(take ~up_to:3 ~sep_by:(char ',') (char 'a')) in
      let r = P.parse input p in
      r = ['a'; 'a'; 'a']
    ]} *)

val take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t
(** [take_while ~sep_by p ~while_ p] returns a parser which encapsulates a list
    of values returned by evaluating parser [p] repeatedly.

    [p] is evaluated only after [while_] evaluates to [true].

    If [sep_by] is specified then the evaluation of [p] must be followed by a
    successful evaluation of [sep_by]. The parsed value of [sep_by] is
    discarded.

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [while_] returns [false]
    - [sep_by] evaluates to failure

    {b Note} [while_] does not consume input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "aab" in
      let p = P.(take_while ~while_:(is_not (char 'b')) (char 'a')) in
      let r = P.parse input p in
      r = ['a'; 'a']

      ;;
      let input = new P.string_input "a,a,ab" in
      let p =
        P.(take_while ~sep_by:(char ',') ~while_:(is_not (char 'b')) (char 'a'))
      in
      let r = P.parse input p in
      r = ['a'; 'a']
    ]} *)

val take_while_cb :
  ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> int t
(** [take_while_on ~sep_by ~while_ ~on_take p] returns a parser which evaluates
    [on_take_cb a] on every successful evaluation of [p]. [a] is the parsed
    value of [p].

    [p] is evaluated only after [while_] evaluates to [true].

    If [sep_by] is specified then the evaluation of [p] must be followed by a
    successful evaluation of [sep_by]. The parsed value of [sep_by] is
    discarded.

    [p] is evaluated repeatedly. The repetition ends when one of the following
    occurs:

    [on_take_cb] is the callback function that is called every time [p] is
    evaluated.

    - [p] evaluates to failure
    - [while_] returns [false]
    - [sep_by] evaluates to failure

    [take_while_cb] is the general version of {!val:take_while}. It allows to
    specify how the value [a] is to be collected.

    {b Note} [while_] does not consume input.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let input = new P.string_input "aaab" in
      let buf = Buffer.create 0 in
      let on_take_cb a = Buffer.add_char buf a in
      let p =
        P.(take_while_cb (char 'a') ~while_:(is_not (char 'b')) ~on_take_cb)
      in
      let r = P.parse input p in
      let s = Buffer.contents buf in
      r = 3 && s = "aaa"
    ]} *)

val not_followed_by : 'a t -> 'b t -> 'a t
(** [not_followed_by a b] Succeeds if parser [p] succeeds and parser [q] fails.
    The second parser [q] never consumes any input. *)

val optional : 'a t -> 'a option t
(** [optional p] parses [p] and retruns [SOME a] if successful. Otherwise
    returns [NONE]. *)

val line : [`LF | `CRLF] -> string t
(** [line] consumes and returns a line of input along with line length. The line
    is delimited by either [\n] or [\r\n].

    {[
      open Reparse.Parse

      let l = parse "line1\r\nline2" (line `CRLF) in
      l = "line1"
    ]} *)

(** {2 Core parsers - RFC 5254, Appending B.1} *)

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

val crlf : string t
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
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end
