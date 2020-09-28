module type S = sig
  type t

  val eof : int -> t -> bool
  (** [eof offset t] returns [true] if [offset] position in [t] represents the
      end of input. *)

  val sub : offset:int -> len:int -> t -> string
  (** [sub t ~offset ~len] reads and returns a string of length [len] from
      position [offset] in input [t]. May return a string of length less than
      [len]. *)

  val nth : int -> t -> char
  (** [nth i t] returns the [i]th char from input [t].

      @raise End_of_file if [i] is the eof. *)
end

(** String input *)
module String : sig
  include S

  val create : string -> t
end = struct
  type t = string

  external create : string -> t = "%identity"

  let eof i s = i >= String.length s
  let sub ~offset ~len s = String.sub s offset len

  let nth i s =
    match s.[i] with
    | c -> c
    | exception Invalid_argument _ -> raise End_of_file
end

(** Unix.file_descr input. Unbuffered. *)
module Unix_fd : sig
  include S

  val create : Unix.file_descr -> t
end = struct
  type t = Unix.file_descr

  external create : Unix.file_descr -> t = "%identity"

  let eof ofs fd = Unix.read fd (Bytes.create 1) ofs 1 = 0

  let rec really_read fd buffer start length =
    if length <= 0 then ()
    else
      match Unix.read fd buffer start length with
      | 0 -> raise End_of_file
      | r -> really_read fd buffer (start + r) (length - r)

  let sub ~offset ~len fd =
    let buf = Bytes.create len in
    really_read fd buf offset len ;
    Bytes.to_string buf

  let nth offset fd =
    let buf = Bytes.create 1 in
    match Unix.read fd buf offset 1 with
    | 0 -> raise End_of_file
    | _ -> Bytes.get buf 0
end
