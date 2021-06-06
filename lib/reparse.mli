(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** {1 Overview} *)

(** Parser provides functions and types to construct robust, performant and
    reusable parsers. At the core is a type {!type:Reparse.t} which represents a
    constructed parser definition. A parser {!type:Reparse.t} is defined by
    composing together one or more parsers or {!type:Reparse.t}s via usage of
    parser operators. An instance of {!type:Reparse.t} represents an
    un-evaluated parser. Use {!val:Reparse.parse} function to evaluate it.
    {!type:Reparse.input} represents a generalization of data input to
    {!val:Reparse.parse}. Implement the interface to create new input types.
    Parser operators - or functions - are broadly organized into following
    categories:

    - Monadic parsers
    - Char/String parsers
    - Alternate parsers
    - Boolean
    - Repetition
    - Others *)

module type PARSER = sig
  (** Represents a parser which can parse value ['a]. Use {{:#parse} parse
      functions} to evaluate a parser. *)
  type 'a t

  type 'a promise

  type input

  val parse : input -> 'a t -> ('a, string) result promise

  (** {2 Monadic operators} *)

  (** [return v] always parses value [v].

      {4:return_examples Examples}

      {[
        module P = Reparse.String.String

        ;;
        let input = new P.string_input "" in
        let v1 = P.(parse input (return 5)) in
        let v2 = P.(parse input (return "hello")) in
        v1 = 5 && v2 = "hello"
      ]} *)
  val return : 'a -> 'a t

  (** [fail err_msg] returns a parser that always fails with [err_msg].

      {4:fail_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let input = new P.string_input "" in
        let r =
          try
            let _ = P.(parse input (fail "hello error")) in
            assert false
          with
          | e -> e
        in
        r
        = P.Parser
            { offset = 0
            ; line_number = 0
            ; column_number = 0
            ; msg = "hello error"
            }
      ]} *)
  val fail : string -> 'a t

  (** [bind f p] is prefix version of [p >>= f]. *)
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  (** [both p q] is evaluates to [(a,b)] when [a] is evaluted from [p] and [b]
      is evaluated from [q]. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** [map f p] is prefix version of [p >>| f]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  module Infix : sig
    (** [p >>= f] returns a new parser b where,

        - [a] is the parsed value of [p]
        - [b] is [f a] Also known as [bind] operation.

        {4:infix_bind_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let f a = P.pure (Char.code a) in
          let p = P.char 'h' in
          let p = p >>= f in
          let v = P.parse_string p "hello" in
          v = 104
        ]} *)
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    (** [p >>| f] returns a new parser encapsulating value [b] where,

        - [a] is the parsed value of [p].
        - [b] is [f a]. Also known as [map] operation.

        {4:infix_map_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let f a = Char.code a in
          let p = P.char 'h' in
          let p = p >>| f in
          let v = P.parse_string p "hello" in
          v = 104
        ]} *)
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    (** [pf <*> q] returns a new parser encapsulating value [b] where

        - [pf] and [q] are evaluated sequentially in order as given.
        - [f] is the parsed value of [pf]
        - [a] is the parsed value of [q]
        - [b] is [f a] Also known as [Applicative] operation.

        {4:infix_applicative_examples Examples}

        {[
          module P = Reparse
          open P

          ;;
          let f a = a + 2 in
          let pf = P.pure f in
          let q = P.pure 2 in
          let p = pf <*> q in
          let v = P.parse_string p "hello" in
          v = 4
        ]} *)
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    (** [f <$> p] is [return f <*> p]. *)
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

    (** [v <$ p] replaces the parse value of [p] with [v].

        {4:infix_replace_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let v = "hello" in
          let p = P.char 'h' in
          let p = v <$ p in
          let v2 = P.parse_string p "hello" in
          v2 = "hello"
        ]} *)
    val ( <$ ) : 'a -> 'b t -> 'a t

    (** [p $> v] is inverse of [v <$ p]. *)
    val ( $> ) : 'a t -> 'b -> 'b t

    (** [p *> q] returns a parser encapsulating value [a] where,

        - [p], [q] are evaluated sequentially in order as given.
        - [a] is parsed value of [q].
        - The parsed value of [p] is discarded. Also known as [discard left].

        {4:infix_discard_left_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let p = P.string "world" in
          let q = P.pure "hello" in
          let p = p *> q in
          let v = P.parse_string p "world" in
          v = "hello"
        ]} *)
    val ( *> ) : _ t -> 'b t -> 'b t

    (** [p <* q] returns a parser encapsulating value [a] where,

        - [p], [q] are evaluated sequentially in order as given.
        - [a] is parsed value of [p].
        - The parsed value of [q] is discarded. Also know as discard_right.

        {4:infix_discard_right_examples Examples}

        {[
          module P = Reparse.String
          open P

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
        - [a] is the parsed value of [q] if [p] is a failure and [q] is a
          success.
        - If both - [p] and [q] - fails, then the parser fails.

        {4:infix_alternate_examples Examples}

        [p] fails and [q] succeeds, therefore we return [q]'s parsed value ['w']

        {[
          module P = Reparse.String
          open P

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
            with
            | _ -> true
          in
          v = true
        ]} *)
    val ( <|> ) : 'a t -> 'a t -> 'a t

    (** [let*] is a let syntax binding for {!val:Reparse.Infix.(>>=)}

        {4:infix_let_bind_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let p =
            let* a = P.pure 5 in
            let total = a + 5 in
            P.pure total
          in
          let v = P.parse_string p "" in
          v = 10
        ]} *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

    (** [let*] is a let syntax binding for {!val:Reparse.(>|=)}

        {4:infix_let_map_examples Examples}

        {[
          module P = Reparse.String
          open P

          ;;
          let p =
            let+ a = P.pure 5 in
            let total = a + 5 in
            total
          in
          let v = P.parse_string p "" in
          v = 10
        ]} *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    (** [p <?> err_msg] parses [p] to value [a] and returns a new parser
        encapsulating [a]. If [p] is a failure, then it fails with error message
        [err_msg]. Often used as a last choice in [<|>], e.g.
        [a <|> b <|> c <?> "expected a b c"].

        {4:infix_error_named_examples Examples}

        {[
          module P = Reparse.String
          open P

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
                { offset = 0
                ; line_number = 0
                ; column_number = 0
                ; msg = "[error]"
                } ->
              true
            | _ -> false
          in
          v = true
        ]} *)
    val ( <?> ) : 'a t -> string -> 'a t
  end

  include module type of Infix

  (** [ppx_let] syntax support module. *)
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

  (** [peek_char t] parses the next character from input without consuming it.
      It fails if [EOI] is reached.

      {4:peek_char_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.peek_char in
        let v = P.parse_string p "hello" in
        v = 'h'
      ]}

      Input is not consumed. *)
  val peek_char : char t

  (** [peek_char_opt] is the exception safe version of [peek_char]. It returns
      an option rather than throwing [exn] when [EOI] is reached. *)
  val peek_char_opt : char option t

  (** [peek_string n] parse a string of length [n] without consuming it.

      {4:peek_string_examples Examples}

      {[
        module P = Reparse.String
        open P

        ;;
        let p = P.peek_string 5 in
        let v = P.parse_string p "hello" in
        v = "hello"
      ]}

      Input is not consumed. *)
  val peek_string : int -> string t

  (** [any_char] parses the next character from input. Fails if input has
      reached end of input.

      {4:next_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let v = P.(parse_string any_char "hello") in
        v = 'h'
      ]} *)
  val any_char : char t

  (** [char c] parses character [c] exactly.

      {4:char_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.char 'h' in
        let v = P.parse_string p "hello" in
        v = 'h'
      ]} *)
  val char : char -> char t

  (** [char_if f] parses a character [c] if [f c] is [true].

      {4:char_if_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p =
          P.char_if (function
            | 'a' -> true
            | _ -> false)
        in
        let v = P.parse_string p "abc" in
        v = 'a'
      ]} *)
  val char_if : (char -> bool) -> char t

  (** [string ~case_sensitive s] parses a string [s] exactly. If
      [case_sensitive] is [false] then comparison is done without character case
      consideration. Default value is [true].

      {4:string_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.string "hello" in
        let v = P.parse_string p "hello world" in
        v = "hello"
      ]} *)
  val string : ?case_sensitive:bool -> string -> string t

  (** [string_of_chars l] converts [char list] [l] to string

      {4:string_of_chars_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.(take ~sep_by:space next >>= string_of_chars) in
        let v = P.parse_string p "h e l l o" in
        v = "hello"
      ]} *)
  val string_of_chars : char list -> string t

  (** [take_string n] returns a string of length [n] exactly from input. *)
  val take_string : int -> string t

  (** {2 Alternate parsers} *)

  (** [any l] parses the value of the first successful parser in list [l].
      Specified parsers in [l] are evaluated sequentially from left to right. A
      failed parser doesn't consume any input, i.e. [offset] is unaffected. The
      parser fails if none of the parsers in [l] are evaluated successfully.

      {4:any_examples Examples}

      First successful parser result is returned

      {[
        module P = Reparse.String

        ;;
        let p = P.(any [ char 'z'; char 'x'; char 'a' ]) in
        let v = P.parse_string p "zabc" in
        v = 'z'

        ;;
        let p = P.(any [ char 'z'; char 'x'; char 'a' ]) in
        let v = P.parse_string p "xabc" in
        v = 'x'

        ;;
        let p = P.(any [ char 'z'; char 'x'; char 'a' ]) in
        let v = P.parse_string p "abc" in
        v = 'a'
      ]}

      Parser fails when none of the parsers in [l] are successful.

      {[
        let p = P.(any [ char 'z'; char 'x'; char 'a' ]) in
        let v =
          try
            let _ = P.parse_string p "yyy" in
            false
          with
          | _ -> true
        in
        v = true
      ]} *)
  val any : ?failure_msg:string -> 'a t list -> 'a t

  (** [alt p q] is [p <|> q]. See {!val:Reparse.Infix.(<|>)} *)
  val alt : 'a t -> 'a t -> 'a t

  (** [optional p] parses [Some a] if successful and [None] otherwise. [a] is
      the parsed value of [p].

      {4:optional_examples Examples}

      {[
        module P = Reparse.String
        open P

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

  (** {2 Boolean} *)

  (** [not_ p] parses value [()] if and only if [p] fails to parse, otherwise
      the parse fails.

      {4:not__examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.(not_ (char 'a')) in
        let v = P.parse_string p "bbb" in
        v = ()
      ]} *)
  val not_ : 'a t -> unit t

  (** [is p] parses [true] if [p] is successful, [false] otherwise. {b Note}
      evaluation of [p] doesn't consume any input.

      {4:is_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.(is (char 'b')) in
        let v = P.parse_string p "bcb" in
        v = true
      ]} *)
  val is : 'a t -> bool t

  (** [is_not p] parses value [true] if [p] fails to parse and [false]
      otherwise. {b Note} evaluating [p] doesn't consume any input.

      {4:is_not_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.(is_not (char 'a')) in
        let v = P.parse_string p "bbb" in
        v = true
      ]} *)
  val is_not : 'a t -> bool t

  (** {2 Repetition} *)

  (** [recur f] returns a recursive parser. Function value [f] accepts a parser
      [p] as its argument and returns a parser [q]. Parser [q] in its definition
      can refer to [p] and [p] can refer to [q] in its own definition. Such
      parsers are also known as a fixpoint or y combinator. *)
  val recur : ('a t -> 'a t) -> 'a t

  (** [all parsers] parses all parsers in [parsers] and returns a list of
      successful parse result. All of the [parsers] must succeed for [all] to
      succeed. *)
  val all : 'a t list -> 'a list t

  (** [skip ~at_least ~up_to p] repeatedly parses [p] and discards its value.
      The lower and upper bound of repetition is specified by arguments
      [at_least] and [up_to] respectively. The default value of [at_least] is 0.
      The default value of [up_to] is unspecified, i.e. there is no upper limit.
      The repetition ends when one of the following occurs:

      - [p] evaluates to failure
      - [up_to] upper bound value is reached The parser encapsulates the count
        of times [p] was evaluated successfully.

      {4:skip_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p = P.(skip space) in
        let v = P.parse_string p "     " in
        v = 5
      ]} *)
  val skip : ?at_least:int -> ?up_to:int -> _ t -> int t

  (** [take ~at_least ~up_to ~sep_by p] repeatedly parses [p] and returns the
      parsed values. The lower and upper bound of repetition is specified by
      arguments [at_least] and [up_to] respectively. The default value of
      [at_least] is [0]. The default value of [up_to] is unspecified, i.e. there
      is no upper limit. If [sep_by] is specified then the evaluation of [p]
      must be followed by a successful evaluation of [sep_by]. The parsed value
      of [sep_by] is discarded. The repetition ends when one of the following
      occurs:

      - [p] evaluates to failure
      - [sep_by] evaluates to failure
      - [up_to] upper boudn value is reached The parser fails if the count of
        repetition of [p] does not match the value specified by [at_least].

      {4:take_examples Examples}

      Default behaviour.

      {[
        module P = Reparse.String

        ;;
        let p = P.(take (char 'a')) in
        let v = P.parse_string p "aaaaa" in
        v = [ 'a'; 'a'; 'a'; 'a'; 'a' ]
      ]}

      Specify [~sep_by].

      {[
        module P = Reparse.String

        ;;
        let p = P.(take ~sep_by:(char ',') (char 'a')) in
        let v = P.parse_string p "a,a,a,a,a" in
        v = [ 'a'; 'a'; 'a'; 'a'; 'a' ]
      ]}

      Specify lower bound argument [at_least].

      {[
        module P = Reparse.String

        ;;
        let p = P.(take ~at_least:3 ~sep_by:(char ',') (char 'a')) in
        let v = P.parse_string p "a,a,a,a,a" in
        v = [ 'a'; 'a'; 'a'; 'a'; 'a' ]
      ]}

      Lower bound not met results in error.

      {[
        module P = Reparse.String

        ;;
        let p = P.(take ~at_least:5 ~sep_by:(char ',') (char 'a')) in
        let v =
          try
            let _ = P.parse_string p "a,a,a,a" in
            false
          with
          | _ -> true
        in
        v = true
      ]}

      Specify upper bound [up_to].

      {[
        module P = Reparse.String

        ;;
        let p = P.(take ~up_to:3 ~sep_by:(char ',') (char 'a')) in
        let v = P.parse_string p "a,a,a,a,a" in
        v = [ 'a'; 'a'; 'a' ]
      ]} *)
  val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t

  (** [take_while_on ~sep_by ~while_ ~on_take p] repeatedly parses [p] and calls
      callback [on_take_cb] with the parsed value. [p] is evaluated if and only
      if [while_] evaluates to [true]. If [sep_by] is specified then the
      evaluation of [p] must be followed by a successful evaluation of [sep_by].
      The parsed value of [sep_by] is discarded. [p] is evaluated repeatedly.
      The repetition ends when one of the following occurs: [on_take_cb] is the
      callback function that is called every time [p] is evaluated.

      - [p] evaluates to failure
      - [while_] returns [false]
      - [sep_by] evaluates to failure [take_while_cb] is the general version of
        {!val:Reparse.take_while}. It allows to specify how the value [a] is to
        be collected. {b Note} [while_] does not consume input.

      {4:take_while_cb_examples Examples}

      {[
        module P = Reparse.String
        open P

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
  val take_while_cb :
    ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> int t

  (** [take_while ~sep_by p ~while_ p] repeatedly parses [p] and returns its
      value. [p] is evaluated if and only if [while_] evaluates to [true]. If
      [sep_by] is specified then the evaluation of [p] must be followed by a
      successful evaluation of [sep_by]. The parsed value of [sep_by] is
      discarded. The repetition ends when one of the following occurs:

      - [p] evaluates to failure
      - [while_] returns [false]
      - [sep_by] evaluates to failure {b Note} [while_] does not consume input.

      {4:take_while_examples Examples}

      Default behaviour.

      {[
        module P = Reparse.String

        ;;
        let p = P.(take_while ~while_:(is_not (char 'b')) (char 'a')) in
        let v = P.parse_string p "aab" in
        v = [ 'a'; 'a' ]
      ]}

      Specify [sep_by].

      {[
        module P = Reparse.String

        ;;
        let p =
          P.(
            take_while ~sep_by:(char ',') ~while_:(is_not (char 'b')) (char 'a'))
        in
        let v = P.parse_string p "a,a,ab" in
        v = [ 'a'; 'a'; 'a' ]
      ]} *)
  val take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t

  (** [take_between ~sep_by ~start ~end_ p] parses [start] and then repeatedly
      parses [p] while the parsed value of [p] doesn't equal to parsed value of
      [end_]. After the repetition end, it parses [end_]. The parser returns the
      list of parsed values of [p]. Both [start] and [end_] parser values are
      discarded. If [sep_by] is specified then the evaluation of [p] must be
      followed by a successful evaluation of [sep_by]. The parsed value of
      [sep_by] is discarded. The repetition ends when one of the following
      occurs:

      - [p] evaluates to failure
      - [end_] parsed value matches [p] parsed value
      - [sep_by] evaluates to failure

      {4:take_between_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let p =
          P.(
            take_between ~sep_by:(char ',') ~start:(P.char '(') ~end_:(char ')')
              next)
        in
        let v = P.parse_string p "(a,a,a)" in
        v = [ 'a'; 'a'; 'a' ]
      ]} *)
  val take_between : ?sep_by:_ t -> start:_ t -> end_:_ t -> 'a t -> 'a list t

  (** {2 Others} *)

  (** [advance n] advances input by [n] bytes. *)
  val advance : int -> unit t

  (** [eoi] parses end of input. Fails if parser is not at end of input.

      {4:eoi_examples Examples}

      {[
        module P = Reparse.String

        ;;
        let v = P.(parse_string eoi "") in
        v = ()

        ;;
        let v =
          try
            let _ = P.(parse_string eoi "a") in
            false
          with
          | _ -> true
        in
        v = true
      ]} *)
  val eoi : unit t
end

module type INPUT = sig
  (** Represents the input. *)
  type t

  (** Represents an input promise. *)
  type 'a promise

  val return : 'a -> 'a promise

  val bind : ('a -> 'b promise) -> 'a promise -> 'b promise

  (** [get t ~pos ~len] returns [`String s] where [String.length s = len] or
      [`Eof] if [EOI] is reached. *)
  val get : t -> pos:int -> len:int -> [ `String of string | `Eof ] promise
end

(** A functor to create parsers based on the given [Input] module. *)
module Make : functor (Input : INPUT) ->
  PARSER with type 'a promise = 'a Input.promise with type input = Input.t

(** A parser when the input is a [string]. *)
module String : PARSER with type 'a promise = 'a with type input = string
