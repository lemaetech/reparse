module type S = sig
  (** {2 Types} *)

  type state

  type +'a t = state -> ok:('a -> unit) -> err:(exn -> unit) -> unit
  (** Represents a parser which can parse value ['a]. *)

  type input

  exception
    Parse_error of
      { offset : int
      ; line_number : int
      ; column_number : int
      ; msg : string }
  (** [Parser_error (lnum, cnum, msg)] Raised by failed parsers. [lnum], [cnum]
      is line number and column number respectively at the time of parser
      failure. [msg] contains a descriptive error message. {b Note} [lnum],
      [cnum] is both [0] if line tracking is disabled. *)

  val parse : ?track_lnum:bool -> input -> 'a t -> 'a
  (** [parse ~tract_lnum input p] executes parser [p] with [input]. If
      [track_lnum] is true then the parser tracks both line and column numbers.
      It is set to [false] by default.

      @raise Parse_error
      {[
        module P = Reparse.String_parser
        open P.Infix

        let p = P.(take next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum)
        let input = Reparse.String_input.create "hello world"

        (* Track line, column number. *)

        ;;
        let r = P.parse ~track_lnum:true input p in
        r = (1, 12)

        (* Don't track line, column number. *)

        ;;
        let r = P.parse input p in
        r = (0, 0)
      ]} *)

  val return : 'a -> 'a t
  (** [return v] returns a parser that always parses value [v].

      {[
        module P = Reparse.String_parser
        open P.Infix

        let input = Reparse.String_input.create ""

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
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(char 'h' >>= fun c -> return (Char.code c)) in
        let input = Reparse.String_input.create "hello" in
        let r = P.parse input p in
        r = 104
      ]} *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** [p >|= f] Mapper parser. Returns a parser which encapsulates value [b] as
      a result of applying [f a] where [a] is the parsed value of [p].

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(char 'h' >|= fun c -> Char.code c) in
        let input = Reparse.String_input.create "hello" in
        let r = P.parse input p in
        r = 104
      ]} *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [pf <*> q] Applicative parser. Returns a parser encapsulating value [b] as
      a result of applying [f a], where [f] is the function value parsed by
      parser [pf] and [a] is the value parsed by [q].

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(return (fun a -> a + 2) <*> return 2) in
        let input = Reparse.String_input.create "hello" in
        let r = P.parse input p in
        r = 4
      ]} *)

  val ( <$ ) : 'b -> 'a t -> 'b t
  (** [v <$ p] replaces the result of [p] with [v].

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.("hello" <$ char 'h') in
        let input = Reparse.String_input.create "hello" in
        let r = P.parse input p in
        r = "hello"
      ]} *)

  (** Mappers over pairs of parsers. Joins their parsing results together. *)

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  (** [f <$> p] returns a parser encapsulating value [b] as a result of applying
      [f a]. [a] is value parsed by [p].

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.((fun a -> a ^ " world") <$> string "hello") in
        let input = Reparse.String_input.create "hello" in
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
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(map2 (fun a b -> a + b) (return 1) (return 2)) in
        let input = Reparse.String_input.create "" in
        let r = P.parse input p in
        r = 3
      ]} *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** [map3 f p q r] returns a parser encapsulating value [d] as a result of
      applying [f a b c]. [a, b, c] are the parsed value of parsers [p], [q] and
      [r] respectively.

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p =
          P.(map3 (fun a b c -> a + b + c) (return 1) (return 2) (return 3))
        in
        let input = Reparse.String_input.create "" in
        let r = P.parse input p in
        r = 6
      ]} *)

  val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  (** [map4 f p q r s] returns a parser encapsulating value [e] as a result of
      applying [f a b c d]. [a, b, c, d] are the parsed value of parsers [p],
      [q], [r] and [s] respectively.

      {[
        module P = Reparse.String_parser
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
        let input = Reparse.String_input.create "" in
        let r = P.parse input p in
        r = 10
      ]} *)

  val ( *> ) : _ t -> 'a t -> 'a t
  (** [p *> q] returns a parser which executes parsers [p] and then [q] and
      encapsulates value [a]. [a] is the parsed value of [q]. The parsed value
      of [p] is discarded.

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(string "world" *> P.return "hello") in
        let input = Reparse.String_input.create "world" in
        let r = P.parse input p in
        r = "hello"
      ]} *)

  val ( <* ) : 'a t -> _ t -> 'a t
  (** [p <* q] similar to [*>]. However, the result of [q] is discarded instead.

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(string "world" <* P.return "hello") in
        let input = Reparse.String_input.create "world" in
        let r = P.parse input p in
        r = "world"
      ]} *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** [p <|> q] Alternate parser. Returns a parser which evaluates both [p] and
      [q] returning [a] and [b] respectively. If [p] succeeds then it returns a
      parser encapsulating [a]. If [p] fails and [q] is a success, then it
      returns a parser encapsulating [b]. If both - [p] and [q] - fails, then
      the parser fails with [Parser_error]

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(char 'h' <|> char 'w') in
        let input = Reparse.String_input.create "world" in
        let r = P.parse input p in
        r = 'w'

        ;;
        let p = P.(char 'h' <|> char 'w') in
        let input = Reparse.String_input.create "hello" in
        let r = P.parse input p in
        r = 'h'

        ;;
        let p = P.(char 'h' <|> char 'w') in
        let input = Reparse.String_input.create "" in
        let r =
          try
            let _ = P.parse input p in
            false
          with _ -> true
        in
        r = true
      ]} *)

  val ( <?> ) : 'a t -> string -> 'a t
  (** [p <?> err_mg] If parser [p] is unable to parse successfully then fails
      with error message [err_msg]. Used as a last choice in [<|>], e.g.
      [a <|> b <|> c <?> "expected a b c"].

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let input = Reparse.String_input.create "" in
        let p = P.next <?> "[error]" in
        let r =
          try
            let _ = P.parse input p in
            false
          with
          | P.Parse_error
              {offset = 0; line_number = 0; column_number = 0; msg = "[error]"}
            ->
              true
          | _ -> false
        in
        r = true
      ]} *)

  val any : 'a t list -> 'a t
  (** [any l] returns a parser encapsulating value [a]. [a] is the parser value
      of the first successfully evaluated parser specified in list [l].
      Specified parsers in [l] are evaluated sequentially from left to right.
      The parser fails if none of the parsers in [l] are evaluated successfully.

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(any [char 'z'; char 'x'; char 'a']) in
        let input = Reparse.String_input.create "abc" in
        let r = P.parse input p in
        r = 'a'

        ;;
        let p = P.(any [char 'z'; char 'x'; char 'a']) in
        let input = Reparse.String_input.create "yyy" in
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
      accumulated by evaluating parsers specified in [l] sequentially - from
      left to right. It only succeeds if and only if all of the parsers in [l]
      succeed.

      {[
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(all [char 'a'; char 'b'; char 'c']) in
        let input = Reparse.String_input.create "abc" in
        let r = P.parse input p in
        r = ['a'; 'b'; 'c']

        ;;
        let p = P.(all [char 'a'; char 'b'; char 'c']) in
        let input = Reparse.String_input.create "abd" in
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
        module P = Reparse.String_parser
        open P.Infix

        ;;
        let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
        let input = Reparse.String_input.create "abc" in
        let r = P.parse input p in
        r = ()

        ;;
        let p = P.(all_unit [char 'a'; char 'b'; char 'c']) in
        let input = Reparse.String_input.create "abd" in
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
  (** [delay f] delays the computation of [p] until it is required. Use it
      together with [<|>].

      {[
        open Reparse.Parse.Parse
        let r =  parse "a" (delay (lazy (char 'z')) <|> delay (lazy (char 'a'))) in
        r = 'a'
      ]} *)

  (* val advance : int -> unit t *)
  (** [advance n] advances parser by the given [n] number of characters. *)

  val is_eoi : bool t
  (** [is_eoi] returns [true] if parser has reached end of input. *)

  val eoi : unit t
  (** [eoi] Parse the end of input to be successful. *)

  (* val fail : (unit -> string) -> 'a t *)
  (** [fail msg] fails the parser with [msg]. *)

  val not_ : 'a t -> unit t
  (** [not_ p] succeeds if and only if [p] fails to parse.

      {[
        open Reparse.Parse

        let p = not_ (char 'a') in
        let r = parse "bbb" p in
        r = ()
      ]} *)

  val is_not : 'a t -> bool t
  (** [is_not p] returns [true] if [p] fails to parse and [false] if [p] is a
      success. Corollary of [not_] combinator. This one however returns a bool
      rather than erroring out itself. {b Note} executing [p] doesn't consume
      any input.

      {[
        open Reparse.Parse

        let r = parse "bbb" (is_not (char 'a')) in
        r = true
      ]} *)

  val is : 'a t -> bool t
  (** [is p] return [true] is [p] parses successfully. Otherwise it return
      [false]. {b Note} executing [p] doesn't consume any input.

      {[
        open Reparse.Parse

        let r = parse "bcb" (is (char 'b')) in
        r = true
      ]} *)

  val lnum : int t
  (** [lnum] return the current line number. The first line number is [1]. *)

  val cnum : int t
  (** [cnum] returns the current column number. The first column number is [1]. *)

  val offset : int t
  (** [offset] returns the current input offset. The first offset is [0]. *)

  val unit : unit t
  (** [unit] is [return ()]. *)

  (** {2 Parsers} *)

  val peek_char : char t
  (** [peek_char t] returns a character from input without consuming it.

      {[
        open Reparse.Parse
        let p = peek_char in
        let r = parse "hello" p in
        r = 'h'

        (* Input offset value remains the same. *)
        let p = peek_char *> offset in
        let r = parse "hello" p in
        r = 0
      ]} *)

  val peek_string : int -> string t
  (** [peek_string n] attempts to retrieve string of length [n] from input
      exactly and return it. No input is consumed.

      {[
        open Reparse.Parse
        let r = parse "hello" (peek_string 5) in
        r = "hello"
      ]} *)

  val next : char t
  (** [next] consumes and returns the next char of input.

      {[
        open Reparse.Parse
        let r = parse "hello" next in
        r = 'h'
      ]} *)

  val char : char -> char t
  (** [char c] accepts character [c] from input exactly.

      {[
        open Reparse.Parse
        let p = char 'h' in
        let r = parse "hello" p in
        r = ()
      ]} *)

  val satisfy : (char -> bool) -> char t
  (** [satisfy f] accepts a char [c] from input if [f c] is true and returns it.

      {[
        open Reparse.Parse
        let p = satisfy (function 'a' -> true | _ -> false) in
        let r = parse "abc" p in
        r = 'a'
      ]} *)

  val string : string -> string t
  (** [string s] accepts [s] exactly.

      {[
        open Reparse.Parse
        let p = string "hello" in
        let r = parse "hello" p in
        r = ()
      ]} *)

  val skip : ?at_least:int -> ?up_to:int -> _ t -> int t
  (** [skip ~at_least ~up_to p] parses [p] zero or more times while discarding
      the result. If [at_least] is given, then [p] must execute successfully
      [at_least] times to be considered successful. Default of [at_least] is 0.
      If [up_to] is given then [p] is executed maximum [up_to] times. By default
      [up_to] doesn't have an upper bound value. Returns the count of times [p]
      was skipped successfully.

      {[
        open Reparse.Parse

        let r = parse "     " (skip space) in
        r = 5
      ]} *)

  val skip_while : _ t -> while_:bool t -> int t

  val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t
  (** [take ~at_least ~up_to ~sep_by p] executes [p] zero or more times up to
      the given upper bound [up_to]. If [at_least] is given, [p] is expected to
      succeed the lower bound of [at_least] times. Default of [at_least] is [0].
      Thre is no upper bound on execution of [p] is [up_to] is not given. If
      [sep_by] is given execution of [p] must be followed by successful
      execution of [sep_by]. Returns the count of times [p] was executed along
      with the list of successfully parsed values.

      {[
        open Reparse.Parse

        let r = parse "aaaaa" (take (char 'a')) in
        r = (5, ['a'; 'a'; 'a'; 'a'; 'a'])
      ]} *)

  val take_while : ?sep_by:_ t -> 'a t -> while_:bool t -> 'a list t
  (** [take_while p ~while_] parses [p] while [while_] is true. It returns the
      count of items taken as well and the items itself.

      {[
        open Reparse.Parse

        let p = take_while (char 'a') ~while_:(is_not (char 'b')) in
        let r =  parse "aab";
        r = (3, ['a';'a';'a'])
      ]} *)

  val take_while_cb :
    ?sep_by:_ t -> 'a t -> while_:bool t -> on_take_cb:('a -> unit) -> int t
  (** [take_while_on p ~while_ ~on_take] parses [p] while [while_] is true. It
      calls [on_take] each time it consumes the input. Returns the count of
      items consumed.

      {[
        open Reparse.Parse
        let buf = Buffer.create 0 in
        let on_take a = Buffer.add_char buf a in
        let p = take_while_on (char 'a') ~while_:(is_not (char 'b')) ~on_take in
        let r = parse "aaab" p in
        let s = Buffer.contents buf in
        r = 3 && s = "aaa"
      ]} *)

  val not_followed_by : 'a t -> 'b t -> 'a t
  (** [not_followed_by a b] Succeeds if parser [p] succeeds and parser [q]
      fails. The second parser [q] never consumes any input. *)

  val optional : 'a t -> 'a option t
  (** [optional p] parses [p] and retruns [SOME a] if successful. Otherwise
      returns [NONE]. *)

  val line : [`LF | `CRLF] -> string t
  (** [line] consumes and returns a line of input along with line length. The
      line is delimited by either [\n] or [\r\n].

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
  (** [lwsp] parse linear whitespaces - *(WSP / CRLF WSP). {b Note} Use of LWSP
      is discouraged. See https://tools.ietf.org/html/rfc5234#appendix-B.1 *)

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
end
