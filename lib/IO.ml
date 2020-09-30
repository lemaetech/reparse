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

  external unsafe_pread : Unix.file_descr -> bytes -> int -> int -> int -> int
    = "caml_pread"
  (** [unsafe_pread fd buf count fd_offset buf_offset] Reads randome bytes from
      [fd] at offset [fd_offset] into [buf] from offset [buf_offset] of length
      [count]. *)

  let nth offset fd =
    let buf = Bytes.create 1 in
    match unsafe_pread fd buf 1 offset 0 with
    | 0 -> raise End_of_file
    | 1 -> Bytes.get buf 0
    | _ -> failwith "IO.Unix_fd.nth"

  let eof ofs fd =
    match nth ofs fd with
    | (_ : char)  -> false
    | exception _ -> true

  let rec really_read fd buf len fd_offset buf_offset =
    if len <= 0 then ()
    else
      match unsafe_pread fd buf len fd_offset buf_offset with
      | 0 -> raise End_of_file
      | r -> really_read fd buf (len - r) (fd_offset + r) (buf_offset + r)

  let sub ~offset ~len fd =
    let buf = Bytes.create len in
    really_read fd buf len offset 0 ;
    Bytes.to_string buf
end
