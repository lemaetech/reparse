module type INPUT = sig
  type t

  type 'a promise

  val return : 'a -> 'a promise

  val bind : ('a -> 'b promise) -> 'a promise -> 'b promise

  val get : t -> pos:int -> len:int -> [ `String of string | `Eof ] promise
end

module type PARSER = sig
  type 'a t

  type 'a promise

  type input

  val parse : input -> 'a t -> ('a, string) result promise

  (** {2 Monadic operators} *)

  val return : 'a -> 'a t

  val fail : string -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

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

  val string : ?case_sensitive:bool -> string -> string t

  val string_of_chars : char list -> string t

  val take_string : int -> string t

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

  val skip : ?at_least:int -> ?up_to:int -> _ t -> int t

  val take : ?at_least:int -> ?up_to:int -> ?sep_by:_ t -> 'a t -> 'a list t

  val take_while_cb :
    ?sep_by:_ t -> while_:bool t -> on_take_cb:('a -> unit) -> 'a t -> int t

  val take_while : ?sep_by:_ t -> while_:bool t -> 'a t -> 'a list t

  val take_between : ?sep_by:_ t -> start:_ t -> end_:_ t -> 'a t -> 'a list t

  (** {2 Others} *)

  val advance : int -> unit t

  val eoi : unit t
end

module Make : functor (Input : INPUT) ->
  PARSER with type 'a promise = 'a Input.promise with type input = Input.t

module String : PARSER with type 'a promise = 'a with type input = string
