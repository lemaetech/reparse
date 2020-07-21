open Sexplib

type error = [ `Msg of string ]

type src = [ `String of string | `Bigstring of Bigstringaf.t ]

type (+'a, +'error) t

val sexp_of_error : error -> Sexp.t

val sexp_of_t : ('a -> Sexp.t) -> ('error -> Sexp.t) -> ('a, 'error) t -> Sexp.t

val advance : int -> (unit, [> error ]) t

val end_of_input : (bool, [> error ]) t
(** [end_of_input] returns [true] if parser has reached end of input. Always
    succeeds. *)

(** {2 Executing} *)

val parse : src -> ('a, ([> error ] as 'b)) t -> ('a, 'b) result

(** {2 Basic Parsers} *)

val char : char -> (char, [> error ]) t

val char_if : (char -> bool) -> (char option, [> error ]) t
(** [char_if f] accepts and returns a [Some c] if [f c] is true. Otherwise it
    returns [None]. Always succeeds. *)

val satisfy : (char -> bool) -> (char, [> error ]) t

val peek_char : (char option, [> error ]) t
(** [peek_char t] returns a character at the current position in the parser.
    Always suceeds and returns [None] if EOF is reached. *)

val peek_char_fail : (char, [> error ]) t

val any_char : (char, [> error ]) t
(** [any_char] accepts any char and returns it. Fails if EOF reached. *)

val peek_string : int -> (string option, [> error ]) t

val string : string -> (string, [> error ]) t

val skip_while : (char -> bool) -> (unit, [> error ]) t
(** [skip_while f] keeps accepting [c] if [f c] is [true]. [c] is discarded.
    Always succeeds. *)

val count_skip_while : (char -> bool) -> (int, [> error ]) t
(** [count_skip_while f] loops through input accepting/advancing [c] only if
    [f c] is true. Returns the count of times the [f c] was true. *)

val count_skip_while_string : int -> (string -> bool) -> (int, [> error ]) t
(** [count_skip_while_string n f] loops through input accepting/advancing [s]
    only if [f s] is true. [String.length s = n]. Returns the count of times the
    [f s] was true. *)

val take_while : (char -> bool) -> (string, [> error ]) t

val take_while_n : int -> (char -> bool) -> (string, [> error ]) t

val not_string : string -> (char, [> `String_matched | error ]) t
(** [not_string s] accepts any string of length [String.length s] which does not
    equal [s]. Conversely equivalent to [string]. *)

(** {2 Constructors} *)

val ok : 'a -> ('a, [> error ]) t

val fail : ([> error ] as 'e) -> (_, 'e) t

(** {2 Combinators} *)

val ( <|> ) : ('a, 'error) t -> ('a, 'error) t -> ('a, 'error) t

val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t

val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t

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
