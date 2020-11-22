(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** {1 Overview} *)

(** Parser provides functions and types to construct robust, performant and
    reusable parsers.

    At the core is a type {!type:t} which represents a constructed parser
    definition. A parser {!type:t} is defined by composing together one or more
    parsers or {!type:t}s via usage of parser operators.

    An instance of {!type:t} represents an un-evaluated parser. Use {!val:parse}
    function to evaluate it.

    {!type:input} represents a generalization of data input to {!val:parse}.
    Implement the interface to create new input types.

    Parser operators - or functions - are broadly organized into following
    categories:

    - Pure
    - Concatentation
    - Alternation
    - Grouping
    - Repetition
    - Optional
    - Query input state
    - Boolean
    - Text
    - RFC 5234 core parsers

    An {{:#infix} Infix} module contains infix and let syntax support functions.

    See {{:#examples} examples} of use. *)

(** {2 Types} *)

(** Represents a parser which can parse value ['a].

    Use {{:#parse} parse functions} to evaluate a parser. *)
type 'a t

(** Represents a generalization of data input source to a parser. Implement this
    interface to provide new sources of input to {!val:parse}. *)
class type input =
  object
    (** [i#eof offset] returns [true] if [offset] position in [i] represents the
        end of input. *)
    method eof : int -> bool

    (** [i#sub t ~offset ~len] reads and returns a string of length [len] at
        position [offset] from input [i]. May return a string of length less
        than [len]. *)
    method sub : offset:int -> len:int -> string

    (** [i#nth n] returns the [n]th char from input [i].

        @raise End_of_file if [n] is at eof. *)
    method nth : int -> char
  end

(** {2:executing_samples Executing Samples} *)

(** Include the [reparse] package in [utop].

    Copy and paste the sample in utop and type [;;] to run it.

    {v #require "reparse";; v} *)

(** {2:parse Parse}

    Evaluate a parser. *)

(** [parse_string ~track_lnum p s] evaluates [p] to value [v] while consuming
    string instance [s].

    If [track_num] is [true] then the parser tracks both the {e line} and the
    {e column} numbers. It is set to [false] by default.

    Line number and column number both start count from [1] if enabled, [0]
    otherwise.

    {i Also see} {!val:lnum} and {!val:cnum}.

    {4:parse_examples Examples}

    Track line and column number

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let s = "hello world" in
      let p = P.(take next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum) in
      let v = P.parse_string ~track_lnum:true p s in
      v = (1, 12)
    ]}

    Default behaviour - doesn't track line, column number.

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let s = "hello world" in
      let p = P.(take next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum) in
      let v = P.parse_string p s in
      v = (0, 0)
    ]}
    @raise Parser when parser encounters error *)
val parse_string : ?track_lnum:bool -> 'a t -> string -> 'a

(** [parse] is a generalised version of {!val:parse_string} over type
    {!type:input}.

    Use this function when you have a custom implementation of {!type:input}. *)
val parse : ?track_lnum:bool -> 'a t -> input -> 'a

(** {2 Exception} *)

(** Raised by parsers which are unable to parse successfully.

    [offset] is the current index position of input at the time of failure.

    [line_number] is line number at the time of failure.

    [column_number] is column number at the time of failure.

    [msg] contains an error description. *)
exception
  Parser of
    { offset : int
    ; line_number : int
    ; column_number : int
    ; msg : string
    }

(** {1 Pure}

    Create parsers from values. *)

(** [pure v] always parses value [v].

    {4:pure_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let input = new P.string_input "" in
      let v1 = P.(parse input (pure 5)) in
      let v2 = P.(parse input (pure "hello")) in
      v1 = 5 && v2 = "hello"
    ]} *)
val pure : 'a -> 'a t

(** [unit] is a convenience function to create a new parser which always parses
    to value [()].

    [unit] is [pure ()]. *)
val unit : unit t

(*(1** {2 Errors} *)

(*    Handle, generate exceptions and failures. *1) *)

(** [fail err_msg] returns a parser that always fails with [err_msg].

    {4:fail_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let input = new P.string_input "" in
      let r =
        try
          let _ = P.(parse input (fail "hello error")) in
          assert false
        with e -> e in
      r
      = P.Parser
          {offset= 0; line_number= 0; column_number= 0; msg= "hello error"}
    ]} *)
val fail : string -> 'a t

(** {1 Concatenation}

    Define parsers by joining two or more parsers. *)

(** {2 Bind} *)

(** [bind p f] returns a new parser [b] where,

    - [a] is the parsed value of [p]
    - [b] is [f a]

    {4:bind_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let f a = P.pure (a ^ " world") in
      let p = P.string "hello" in
      let p = P.bind p f in
      let input = new P.string_input "hello" in
      let b = P.parse input p in
      b = "hello world"
    ]}

    See {!Infix.(>>=)}. [p >>= f] is the infix equivalent of [bind p f]. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** {2 Map}

    Mappers transform from one parser value to another. [map] functions
    [map2, map3, map4] are defined in terms of {!val:bind}s. So a given mapper
    function usage can be defined equivalently in terms of {!val:bind}s. *)

(** [map f p] returns a new parser encapsulating value [b] where,

    - [a] is the parsed value of [p].
    - [b] is [f a].

    {4:map_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let f a = a ^ " world" in
      let p = P.string "hello" in
      let p = P.map f p in
      let b = P.parse p "hello" in
      b = "hello world"
    ]}

    Since [map] is defined in terms of [bind], the above usage of [map] is
    equivalent to the [bind] usage below,

    {[
      module P = Reparse.Parser

      ;;
      let f a = P.pure (a ^ " world") in
      let p = P.string "hello" in
      let p = P.bind p f in
      let r = P.parse_string p "hello" in
      r = "hello world"
    ]}

    See {!Infix.(<$>)}. [f <$> p] is infix equivalent of [map f p]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [map2 f p q] returns a new parser encapsulating value [c] where,

    - [p] and [q] are evaluated sequentially in order as given.
    - [a, b] are the parsed values of parsers [p] and [q] respectively.
    - [c] is [f a b].

    {4:map2_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let f a b = a + b in
      let p = P.pure 1 in
      let q = P.pure 2 in
      let p = P.map2 f p q in
      let v = P.parse_string p "" in
      v = 3
    ]}

    The above usage of [map2] is equivalent to below,

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.pure 1 >>= fun a -> P.pure 2 >>= fun b -> P.pure (a + b) in
      let v = P.parse_string p "" in
      v = 3
    ]} *)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** [map3 f p q r] returns a new parser encapsulating value [d] where,

    - [p], [q], [r] are evaluated sequentially in order as given.
    - [a, b, c] are the parsed values of parsers [p], [q] and [r] respectively.
    - [d] is [f a b c].

    {4:map3_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let f a b c = a + b + c in
      let p = P.pure 1 in
      let q = P.pure 2 in
      let r = P.pure 3 in
      let p = P.map3 f p q r in
      let v = P.parse_string p "" in
      v = 6
    ]} *)
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

(** [map4 f p q r s] returns a new parser encapsulating value [e] where,

    - [p], [q], [r] and [s] are evaluated sequentially in order as given.
    - [a, b, c, d] are the parsed values of parsers [p], [q], [r] and [s]
      respectively.
    - [e] is [f a b c d].

    {4:map4_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let f a b c d = a + b + c + d in
      let p = P.pure 1 in
      let q = P.pure 2 in
      let r = P.pure 3 in
      let s = P.pure 4 in
      let p = P.map4 f p q r s in
      let v = P.parse_string p "" in
      v = 10
    ]} *)
val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

(** [delay p] returns a parser which lazily parses [p].

    {4:delay_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(delay (lazy (char 'z')) <|> delay (lazy (char 'a'))) in
      let v = P.parse_string p "abc" in
      v = 'a'
    ]} *)
val delay : 'a t Lazy.t -> 'a t

(** [named name p] uses [name] as part of an error message when constructing
    exception {!exception:Parser} if parse of [p] fails.

    Also see {!val:Infix.(<?>)}

    {4:named_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(char 'a' |> named "parse_c") in
      let v =
        try
          let _ = P.parse_string p "zzd" in
          assert false
        with e -> e in
      v
      = P.Parser
          { offset= 0
          ; line_number= 0
          ; column_number= 0
          ; msg=
              "[parse_c] Reparse.Parser.Parser(0, 0, 0, \"[char] expected \
               'a'\")" }
    ]} *)
val named : string -> 'a t -> 'a t

(** {1 Alternation}

    One or the other. *)

(** [any l] parses the value of the first successful parser in list [l].

    Specified parsers in [l] are evaluated sequentially from left to right. A
    failed parser doesn't consume any input, i.e. [offset] is unaffected.

    The parser fails if none of the parsers in [l] are evaluated successfully.

    {4:any_examples Examples}

    First successful parser result is returned

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let v = P.parse_string p "zabc" in
      v = 'z'

      ;;
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let v = P.parse_string p "xabc" in
      v = 'x'

      ;;
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let v = P.parse_string p "abc" in
      v = 'a'
    ]}

    Parser fails when none of the parsers in [l] are successful.

    {[
      let p = P.(any [char 'z'; char 'x'; char 'a']) in
      let v =
        try
          let _ = P.parse_string p "yyy" in
          false
        with _ -> true in
      v = true
    ]} *)
val any : 'a t list -> 'a t

(** [alt p q] is [p <|> q].

    See {!val:Infix.(<|>)} *)
val alt : 'a t -> 'a t -> 'a t

(** {1 Grouping}

    Group parsers. *)

(** [all l] parses all parsers in [l] and returns the parsed values.

    The parser only succeeds if and only if all of the parsers in [l] succeed.

    Parsers in [l] are evaluated sequentially - from left to right.

    {4:all_examples Examples}

    All specified parsers succeed.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(all [char 'a'; char 'b'; char 'c']) in
      let v = P.parse_string p "abc" in
      v = ['a'; 'b'; 'c']
    ]}

    One of the specified parsers - [char 'c'] fails.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(all [char 'a'; char 'b'; char 'c']) in
      let v =
        try
          let _ = P.parse_string p "abd" in
          false
        with _ -> true in
      v = true
    ]} *)
val all : 'a t list -> 'a list t

(** [all_unit l] parses all parsers in [l] while discarding the parsed values.

    {4:all_unit_examples Examples}

    All specified parsers succeed.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
      let v = P.parse_string p "abc" in
      v = ()
    ]}

    One of the specified parsers - [char 'c'] - fails.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
      let v =
        try
          let _ = P.parse_string p "abd" in
          false
        with _ -> true in
      v = true
    ]} *)
val all_unit : 'a t list -> unit t

(** {1 Repetition} *)

(** {2 Recur} *)

(** [recur f] returns a recursive parser. Function value [f] accepts a parser
    [p] as its argument and returns a parser [q]. Parser [q] in its definition
    can refer to [p] and [p] can refer to [q] in its own definition.

    Such parsers are also known as a fixpoint or y combinator. *)
val recur : ('a t -> 'a t) -> 'a t

(** {2 Skip}

    Discards parsed values. *)

(** [skip ~at_least ~up_to p] repeatedly parses [p] and discards its value.

    The lower and upper bound of repetition is specified by arguments [at_least]
    and [up_to] respectively. The default value of [at_least] is 0. The default
    value of [up_to] is unspecified, i.e. there is no upper limit.

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [up_to] upper bound value is reached

    The parser encapsulates the count of times [p] was evaluated successfully.

    {4:skip_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(skip space) in
      let v = P.parse_string p "     " in
      v = 5
    ]} *)
val skip : ?at_least:int -> ?up_to:int -> _ t -> int t

(** [skip_while p ~while_] repeatedly parses [p] and discards its value if
    parser [while_] parses to value [true].

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [while_] returns [false]

    {b Note} [while_] does not consume input.

    The parser encapsulates the count of times [p] was evaluated successfully.

    {4:skip_while_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(skip_while next ~while_:(is space)) in
      let v = P.parse_string p "     " in
      v = 5
    ]} *)
val skip_while : _ t -> while_:bool t -> int t

(** {2 Take}

    Collects parsed values *)

(** [take ~at_least ~up_to ~sep_by p] repeatedly parses [p] and returns the
    parsed values.

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

    {4:take_examples Examples}

    Default behaviour.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take (char 'a')) in
      let v = P.parse_string p "aaaaa" in
      v = ['a'; 'a'; 'a'; 'a'; 'a']
    ]}

    Specify [~sep_by].

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take ~sep_by:(char ',') (char 'a')) in
      let v = P.parse_string p "a,a,a,a,a" in
      v = ['a'; 'a'; 'a'; 'a'; 'a']
    ]}

    Specify lower bound argument [at_least].

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take ~at_least:3 ~sep_by:(char ',') (char 'a')) in
      let v = P.parse_string p "a,a,a,a,a" in
      v = ['a'; 'a'; 'a'; 'a'; 'a']
    ]}

    Lower bound not met results in error.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take ~at_least:5 ~sep_by:(char ',') (char 'a')) in
      let v =
        try
          let _ = P.parse_string p "a,a,a,a" in
          false
        with _ -> true in
      v = true
    ]}

    Specify upper bound [up_to].

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take ~up_to:3 ~sep_by:(char ',') (char 'a')) in
      let v = P.parse_string p "a,a,a,a,a" in
      v = ['a'; 'a'; 'a']
    ]} *)
val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t

(** [take_while ~sep_by p ~while_ p] repeatedly parses [p] and returns its
    value.

    [p] is evaluated if and only if [while_] evaluates to [true].

    If [sep_by] is specified then the evaluation of [p] must be followed by a
    successful evaluation of [sep_by]. The parsed value of [sep_by] is
    discarded.

    The repetition ends when one of the following occurs:

    - [p] evaluates to failure
    - [while_] returns [false]
    - [sep_by] evaluates to failure

    {b Note} [while_] does not consume input.

    {4:take_while_examples Examples}

    Default behaviour.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take_while ~while_:(is_not (char 'b')) (char 'a')) in
      let v = P.parse_string p "aab" in
      v = ['a'; 'a']
    ]}

    Specify [sep_by].

    {[
      module P = Reparse.Parser

      ;;
      let p =
        P.(take_while ~sep_by:(char ',') ~while_:(is_not (char 'b')) (char 'a'))
      in
      let v = P.parse_string p "a,a,ab" in
      v = ['a'; 'a'; 'a']
    ]} *)
val take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t

(** [take_while_on ~sep_by ~while_ ~on_take p] repeatedly parses [p] and calls
    callback [on_take_cb] with the parsed value.

    [p] is evaluated if and only if [while_] evaluates to [true].

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

    {4:take_while_cb_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let buf = Buffer.create 0 in
      let on_take_cb a = Buffer.add_char buf a in
      let p =
        P.(take_while_cb (char 'a') ~while_:(is_not (char 'b')) ~on_take_cb)
      in
      let v = P.parse_string p "aaab" in
      let s = Buffer.contents buf in
      v = 3 && s = "aaa"
    ]} *)
val take_while_cb
  :  ?sep_by:_ t
  -> while_:bool t
  -> on_take_cb:('a -> unit)
  -> 'a t
  -> int t

(** {1 Optional}

    Don't fail when parsing is not successful.*)

(** [optional p] parses [Some a] if successful and [None] otherwise. [a] is the
    parsed value of [p].

    {4:optional_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(optional (char 'a')) in
      let v = P.parse_string p "ab" in
      v = Some 'a'

      ;;
      let p = P.(optional (char 'z')) in
      let v = P.parse_string p "ab" in
      v = None
    ]}*)
val optional : 'a t -> 'a option t

(** {1 Query Input state} *)

(** [is_eoi] parses to [true] if parser has reached end of input, [false]
    otherwise.

    {4:is_eoi_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string is_eoi "") in
      v = true

      ;;
      let v = P.(parse_string is_eoi "a") in
      v = false
    ]} *)
val is_eoi : bool t

(** [eoi] parses end of input. Fails if parser is not at end of input.

    {4:eoi_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string eoi "") in
      v = ()

      ;;
      let v =
        try
          let _ = P.(parse_string eoi "a") in
          false
        with _ -> true in
      v = true
    ]} *)
val eoi : unit t

(** [lnum] parses the current line number of input. line number count start form
    [1].

    {4:lnum_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(next *> lnum) in
      let v = P.parse_string ~track_lnum:true p "bcb" in
      v = 1
    ]} *)
val lnum : int t

(** [cnum] parses the current column number. column number count start from [1].

    {4:cnum_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(next *> cnum) in
      let v = P.parse_string ~track_lnum:true p "bcb" in
      v = 2
    ]} *)
val cnum : int t

(** [offset] parses the current input offset. offset count start from [0].

    {4:offset_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(next *> offset) in
      let v = P.parse_string ~track_lnum:true p "bcb" in
      v = 1
    ]} *)
val offset : int t

(** {1 Boolean}

    [true], [false], is, is not. *)

(** [not_ p] parses value [()] if and only if [p] fails to parse, otherwise the
    parse fails.

    {4:not__examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(not_ (char 'a')) in
      let v = P.parse_string p "bbb" in
      v = ()
    ]} *)
val not_ : 'a t -> unit t

(** [not_followed_by p q] parses value of [p] only if immediate and subsequent
    parse of [q] is a failure. Parser [q] doesn't consumes any input.

    {4:not_followed_by_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(not_followed_by (char 'a') (char 'a')) in
      let v = P.parse_string p "ab" in
      v = 'a'
    ]}*)
val not_followed_by : 'a t -> 'b t -> 'a t

(** [is_not p] parses value [true] if [p] fails to parse and [false] otherwise.
    {b Note} evaluating [p] doesn't consume any input.

    {4:is_not_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(is_not (char 'a')) in
      let v = P.parse_string p "bbb" in
      v = true
    ]} *)
val is_not : 'a t -> bool t

(** [is p] parses [true] if [p] is successful, [false] otherwise. {b Note}
    evaluation of [p] doesn't consume any input.

    {4:is_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(is (char 'b')) in
      let v = P.parse_string p "bcb" in
      v = true
    ]} *)
val is : 'a t -> bool t

(** {1 Text}

    Text parsing. *)

(** [peek_char t] parses the next character from input without consuming it.

    {4:peek_char_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.peek_char in
      let v = P.parse_string p "hello" in
      v = 'h'
    ]}

    Input is not consumed.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(peek_char *> offset) in
      let v = P.parse_string p "hello" in
      v = 0
    ]} *)
val peek_char : char t

(** [peek_string n] parse a string of length [n] without consuming it.

    {4:peek_string_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.peek_string 5 in
      let v = P.parse_string p "hello" in
      v = "hello"
    ]}

    Input is not consumed.

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(peek_string 5 *> offset) in
      let v = P.parse_string p "hello" in
      v = 0
    ]} *)
val peek_string : int -> string t

(** [next] parses the next character from input. Fails if input has reached end
    of input.

    {4:next_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string next "hello") in
      v = 'h'
    ]} *)
val next : char t

(** [char c] parses character [c] exactly.

    {4:char_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.char 'h' in
      let v = P.parse_string p "hello" in
      v = 'h'
    ]} *)
val char : char -> char t

(** [char_if f] parses a character [c] if [f c] is [true].

    {4:char_if_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.char_if (function 'a' -> true | _ -> false) in
      let v = P.parse_string p "abc" in
      v = 'a'
    ]} *)
val char_if : (char -> bool) -> char t

(** [string ~case_sensitive s] parses a string [s] exactly. 

    If [case_sensitive] is [false] then comparison is done without 
    character case consideration. Default value is [true].

    {4:string_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.string "hello" in
      let v = P.parse_string p "hello world" in
      v = "hello"
    ]} *)
val string : ?case_sensitive:bool -> string -> string t

(** [line c] parses a line of text from input.

    Line delimiter [c] can be either [`LF] or [`CRLF]. This corresponds to [\n]
    or [\r\n] character respectively.

    {4:line_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.line `CRLF in
      let v = P.parse_string p "line1\r\nline2" in
      v = "line1"
    ]} *)
val line : [ `LF | `CRLF ] -> string t

(** {1:rfc5234 RFC 5234}

    Parsers as defined in RFC 5234, Appendix B.1.

    @see <https://tools.ietf.org/html/rfc5234#appendix-B> *)

(** [alpha] parses a character in range [A- Z] or [a-z].

    {4:alpha_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(take alpha) in
      let v = P.parse_string p "abcdABCD" in
      v = ['a'; 'b'; 'c'; 'd'; 'A'; 'B'; 'C'; 'D']
    ]} *)
val alpha : char t

(** [alpha_num] parses a character in range [A-Z] or [a-z] or [0-9].

    {4:alpha_num_examples Examples}

    {[
      module P = Reparse.Parser
      open P.Infix

      ;;
      let p = P.(take alpha_num) in
      let v = P.parse_string p "ab123ABCD" in
      v = ['a'; 'b'; '1'; '2'; '3'; 'A'; 'B'; 'C'; 'D']
    ]} *)
val alpha_num : char t

(** [bit] parses a character which is either ['0'] or ['1'].

    {4:bit_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take bit) in
      let v = P.parse_string p "0110 ab" in
      v = ['0'; '1'; '1'; '0']
    ]} *)
val bit : char t

(** [ascii_char] parses any US-ASCII character.

    {4:ascii_char_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take ascii_char) in
      let v = P.parse_string p "0110 abc '" in
      v = ['0'; '1'; '1'; '0'; ' '; 'a'; 'b'; 'c'; ' '; '\'']
    ]} *)
val ascii_char : char t

(** [cr] parses character ['\r'].

    {4:cr_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string cr "\rab") in
      v = '\r'
    ]} *)
val cr : char t

(** [crlf] parses string ["\r\n"].

    {4:crlf_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string crlf "\r\n abc") in
      v = "\r\n"
    ]} *)
val crlf : string t

(** [control] parses characters in range [0x00 - 0x1F] or character [0x7F].

    {4:control_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string control "\x00") in
      v = '\x00'
    ]} *)
val control : char t

(** [digit] parses one of the digit characters, [0 .. 9].

    {4:digit_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take digit) in
      let v = P.parse_string p "0123456789a" in
      v = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
    ]} *)
val digit : char t

(** [digits] parses one or more digit characters, [0 .. 9].

    {4:digits_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string digits "1234 +") in
      v = "1234"
    ]} *)
val digits : string t

(** [dquote] parses double quote character ['"'].

    {4:dquote_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string dquote "\"hello ") in
      v = '"'
    ]} *)
val dquote : char t

(** [hex_digit] parses any of the hexadecimal digits - [0..9, A, B, C, D, E, F].

    {4:hex_digit_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take hex_digit) in
      let v = P.parse_string p "0ABCDEFa" in
      v = ['0'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F']
    ]} *)
val hex_digit : char t

(** [htab] parses a horizontal tab character ['\t'].

    {4:htab_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string htab "\t") in
      v = '\t'
    ]} *)
val htab : char t

(** [lf] parses a linefeed ['\n'] character.

    {4:lf_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string lf "\n") in
      v = '\n'
    ]} *)
val lf : char t

(** [octect] parses any character in the range [\x00 - \xFF]. Synonym for
    {!val:next}

    {4:octet_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take octet) in
      let v = P.parse_string p "0110 abc '" in
      v = ['0'; '1'; '1'; '0'; ' '; 'a'; 'b'; 'c'; ' '; '\'']
    ]} *)
val octet : char t

(** [space] parses a space character.

    {4:space_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string space " abc '") in
      v = ' '
    ]} *)
val space : char t

(** [spaces] parses one or more spaces.

    {4:spaces_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let v = P.(parse_string spaces "   abc") in
      v = [' '; ' '; ' ']
    ]} *)
val spaces : char list t

(** [vchar] parses any of the visible - printable - characters.

    {4:vchar_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take vchar) in
      let v = P.parse_string p "0110abc\x00" in
      v = ['0'; '1'; '1'; '0'; 'a'; 'b'; 'c']
    ]} *)
val vchar : char t

(** [whitespace] parses a space [' '] or horizontal tab ['\t'] character.

    {4:whitespace_examples Examples}

    {[
      module P = Reparse.Parser

      ;;
      let p = P.(take whitespace) in
      let v = P.parse_string p "\t \t " in
      v = ['\t'; ' '; '\t'; ' ']
    ]} *)
val whitespace : char t

(** {1:infix Infix} *)

(** Provides functions to support infix and let syntax operators.

    Open the module to use it:

    {[ open Reparse.Parser.Infix ]} *)
module Infix : sig
  (** [p >>= f] returns a new parser b where,

      - [a] is the parsed value of [p]
      - [b] is [f a]

      Also known as [bind] operation.

      {4:infix_bind_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let f a = P.pure (Char.code a) in
        let p = P.char 'h' in
        let p = p >>= f in
        let v = P.parse_string p "hello" in
        v = 104
      ]} *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [p >|= f] returns a new parser encapsulating value [b] where,

      - [a] is the parsed value of [p].
      - [b] is [f a].

      Also known as [map] operation.

      {4:infix_map_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let f a = Char.code a in
        let p = P.char 'h' in
        let p = p >|= f in
        let v = P.parse_string p "hello" in
        v = 104
      ]} *)
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

  (** [pf <*> q] returns a new parser encapsulating value [b] where

      - [pf] and [q] are evaluated sequentially in order as given.
      - [f] is the parsed value of [pf]
      - [a] is the parsed value of [q]
      - [b] is [f a]

      Also known as [Applicative] operation.

      {4:infix_applicative_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let f a = a + 2 in
        let pf = P.pure f in
        let q = P.pure 2 in
        let p = pf <*> q in
        let v = P.parse_string p "hello" in
        v = 4
      ]} *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (** [v <$ p] replaces the parse value of [p] with [v].

      {4:infix_replace_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let v = "hello" in
        let p = P.char 'h' in
        let p = v <$ p in
        let v2 = P.parse_string p "hello" in
        v2 = "hello"
      ]} *)
  val ( <$ ) : 'b -> 'a t -> 'b t

  (** [f <$> p] returns a parser encapsulating value [b] where,

      - [a] is the parsed value of [p]
      - [b] is [f a]

      This is the infix version of {!val:map}.

      {4:infix_mapper_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let f a = a ^ " world" in
        let p = P.string "hello" in
        let p = f <$> p in
        let v = P.parse_string p "hello" in
        v = "hello world"
      ]} *)
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  (** [p *> q] returns a parser encapsulating value [a] where,

      - [p], [q] are evaluated sequentially in order as given.
      - [a] is parsed value of [q].
      - The parsed value of [p] is discarded.

      Also known as [discard left].

      {4:infix_discard_left_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p = P.string "world" in
        let q = P.pure "hello" in
        let p = p *> q in
        let v = P.parse_string p "world" in
        v = "hello"
      ]} *)
  val ( *> ) : _ t -> 'a t -> 'a t

  (** [p <* q] returns a parser encapsulating value [a] where,

      - [p], [q] are evaluated sequentially in order as given.
      - [a] is parsed value of [p].
      - The parsed value of [q] is discarded.

      Also know as discard_right.

      {4:infix_discard_right_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p = P.string "world" in
        let q = P.pure "hello" in
        let p = p <* q in
        let v = P.parse_string p "world" in
        v = "world"
      ]} *)
  val ( <* ) : 'a t -> _ t -> 'a t

  (** [p <|> q] returns a parser encapsulating value [a] where,

      - [p],[q] are evaluated sequentially in order as given.
      - [a] is the parsed value of [p] if [p] is successful
      - [a] is the parsed value of [q] if [p] is a failure and [q] is a success.
      - If both - [p] and [q] - fails, then the parser fails.

      {4:infix_alternate_examples Examples}

      [p] fails and [q] succeeds, therefore we return [q]'s parsed value ['w']

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p = P.char 'h' in
        let q = P.char 'w' in
        let p = p <|> q in
        let v = P.parse_string p "world" in
        v = 'w'
      ]}

      [p] succeeds therefore we return its parsed value ['h']

      {[
        let p = P.char 'h' in
        let q = P.char 'w' in
        let p = p <|> q in
        let v = P.parse_string p "hello" in
        v = 'h'
      ]}

      The parser fails if both [p] and [q] fails.

      {[
        let p = P.char 'h' in
        let q = P.char 'w' in
        let p = p <|> q in
        let v =
          try
            let _ = P.parse_string p "" in
            false
          with _ -> true in
        v = true
      ]} *)
  val ( <|> ) : 'a t -> 'a t -> 'a t

  (** [p <?> err_msg] parses [p] to value [a] and returns a new parser
      encapsulating [a]. If [p] is a failure, then it fails with error message
      [err_msg].

      Often used as a last choice in [<|>], e.g.
      [a <|> b <|> c <?> "expected a b c"].

      {4:infix_error_named_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p = P.char 'h' <|> P.char 'w' in
        let err_msg = "[error]" in
        let p = p <?> err_msg in
        let v =
          try
            let _ = P.parse_string p "" in
            false
          with
          | P.Parser
              {offset= 0; line_number= 0; column_number= 0; msg= "[error]"} ->
              true
          | _ -> false in
        v = true
      ]} *)
  val ( <?> ) : 'a t -> string -> 'a t

  (** [let*] is a let syntax binding for {!val:(>>=)}

      {4:infix_let_bind_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p =
          let* a = P.pure 5 in
          let total = a + 5 in
          P.pure total in
        let v = P.parse_string p "" in
        v = 10
      ]} *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [let*] is a let syntax binding for {!val:(>|=)}

      {4:infix_let_map_examples Examples}

      {[
        module P = Reparse.Parser
        open P.Infix

        ;;
        let p =
          let+ a = P.pure 5 in
          let total = a + 5 in
          total in
        let v = P.parse_string p "" in
        v = 10
      ]} *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

(** {1:examples Examples} *)

(** {2 Calculator} *)

(** An example calculator that supports [+,-,*] and [/] calculations.

    The expression grammar is defined by the following BNF grammar:

    {v
<expr>   ::= <term>   "+" <expr> 
           | <term>
<term>   ::= <factor> "*" <term> 
           | <factor>
<factor> ::= "(" <expr> ")" 
           | integer
    v}
    {[
      module P = Reparse.Parser
      open P.Infix

      type expr =
        | Int  of int
        | Add  of expr * expr
        | Sub  of expr * expr
        | Mult of expr * expr
        | Div  of expr * expr

      let skip_spaces = P.skip P.space

      let binop : 'a P.t -> char -> 'b P.t -> ('a -> 'b -> 'c) -> 'c P.t =
       fun exp1 op exp2 f ->
        P.map3
          (fun e1 _ e2 -> f e1 e2)
          exp1
          (skip_spaces *> P.char op <* skip_spaces)
          exp2

      let integer : expr P.t =
        let+ d = P.digits in
        Int (int_of_string d)

      let factor : expr P.t -> expr P.t =
       fun expr ->
        P.any
          [ P.char '(' *> skip_spaces *> expr <* skip_spaces <* P.char ')'
          ; skip_spaces *> integer <* skip_spaces ]

      let term : expr P.t -> expr P.t =
       fun factor ->
        P.recur (fun term ->
            let mult = binop factor '*' term (fun e1 e2 -> Mult (e1, e2)) in
            let div = binop factor '/' term (fun e1 e2 -> Div (e1, e2)) in
            mult <|> div <|> factor )

      let expr : expr P.t =
        P.recur (fun expr ->
            let factor = factor expr in
            let term = term factor in
            let add = binop term '+' expr (fun e1 e2 -> Add (e1, e2)) in
            let sub = binop term '-' expr (fun e1 e2 -> Sub (e1, e2)) in
            P.any [add; sub; term] )

      let rec eval : expr -> int = function
        | Int i         -> i
        | Add (e1, e2)  -> eval e1 + eval e2
        | Sub (e1, e2)  -> eval e1 - eval e2
        | Mult (e1, e2) -> eval e1 * eval e2
        | Div (e1, e2)  -> eval e1 / eval e2

      (* Test AST *)
      let r =
        let actual = P.parse_string expr "1*2-4+3" in
        let expected = Sub (Mult (Int 1, Int 2), Add (Int 4, Int 3)) in
        Bool.equal (expected = actual) true

      (* Run the evaluator. *)
      let exp_result = eval (P.parse_string expr "12+1*10") |> Int.equal 22
    ]} *)

(** {2 Json} *)

(** Implements JSON parser as defined in https://tools.ietf.org/html/rfc8259.

    Assumes UTF-8 character encoding. However, it doesn't do any validation.

    Sample top_level inputs;

    {v
  parse json_value "true";;
  parse json_value "false";;
  parse json_value "null";;
  parse json_value "123";;
  parse json_value "123.345";;
  parse json_value "123e123";;
  parse json_value "123.33E123";;
  parse json_value {|{"field1": 123,"field2": "value2"}|};;
  parse json_value {|{"field1":[123,"hello",-123.23], "field2":123} |};;
  parse json_value {|{"field1":123, "field2":123} |};;
  parse json_value {|[123,"hello",-123.23, 123.33e13, 123E23] |};;
    v}
    {[
      module P = Reparse.Parser
      open P.Infix

      type value =
        | Object of (string * value) list
        | Array  of value list
        | Number of
            { negative: bool
            ; int: string
            ; frac: string option
            ; exponent: string option }
        | String of string
        | False
        | True
        | Null

      let ws =
        P.skip
          (P.char_if (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false))

      let implode l = List.to_seq l |> String.of_seq
      let struct_char c = ws *> P.char c <* ws
      let null_value = ws *> P.string "null" *> ws *> P.pure Null
      let false_value = ws *> P.string "false" *> ws *> P.pure False
      let true_value = ws *> P.string "true" *> ws *> P.pure True
      let sprintf = Printf.sprintf

      let number_value =
        let* negative =
          P.optional (P.char '-') >|= function Some '-' -> true | _ -> false
        in
        let* int =
          let digits1_to_9 =
            P.char_if (function '1' .. '9' -> true | _ -> false) in
          let num =
            P.map2
              (fun first_ch digits -> sprintf "%c%s" first_ch digits)
              digits1_to_9 P.digits in
          P.any [P.string "0"; num]
        in
        let* frac = P.optional (P.char '.' *> P.digits) in
        let+ exponent =
          P.optional
            (let* e = P.char 'E' <|> P.char 'e' in
             let* sign = P.optional (P.char '-' <|> P.char '+') in
             let sign =
               match sign with Some c -> sprintf "%c" c | None -> "" in
             let+ digits = P.digits in
             sprintf "%c%s%s" e sign digits)
        in
        Number {negative; int; frac; exponent}

      let string =
        let escaped =
          let ch =
            P.char '\\'
            *> P.char_if (function
                 | '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> true
                 | _ -> false )
            >|= sprintf "\\%c" in
          let hex4digit =
            let+ hex =
              P.string "\\u" *> P.take ~at_least:4 ~up_to:4 P.hex_digit
              >|= implode
            in
            sprintf "\\u%s" hex in
          P.any [ch; hex4digit] in
        let unescaped =
          P.take_while
            ~while_:(P.is_not (P.any [P.char '\\'; P.control; P.dquote]))
            P.next
          >|= implode in
        let+ str =
          P.dquote *> P.take (P.any [escaped; unescaped]) <* P.dquote
        in
        String.concat "" str

      let string_value = string >|= fun s -> String s

      let json_value =
        P.recur (fun value ->
            let value_sep = struct_char ',' in
            let object_value =
              let member =
                let* nm = string <* struct_char ':' in
                let+ v = value in
                (nm, v) in
              let+ object_value =
                struct_char '{' *> P.take member ~sep_by:value_sep
                <* struct_char '}'
              in
              Object object_value in
            let array_value =
              let+ vals =
                struct_char '[' *> P.take value ~sep_by:value_sep
                <* struct_char ']'
              in
              Array vals in
            P.any
              [ object_value; array_value; number_value; string_value
              ; false_value; true_value; null_value ] )

      let parse s = P.parse_string json_value s
    ]} *)
