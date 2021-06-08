type stream_input =
  { stream : char Lwt_stream.t
  ; buf : Buffer.t
  ; mutable committed : int (* count of chars already processed by parser. *)
  }

let create_stream_input stream =
  { stream; buf = Buffer.create 0; committed = 0 }

module Stream = Reparse.Make (struct
  open Lwt.Infix

  type 'a promise = 'a Lwt.t

  type t = stream_input

  let return = Lwt.return

  let bind f p = Lwt.bind p f

  let get t ~pos ~len =
    if len < 0 then raise (invalid_arg "len");
    if pos < 0 || pos < t.committed then invalid_arg "pos";

    let pos' = pos - t.committed in
    let len' = Buffer.length t.buf - (pos' + len) in
    if len' >= 0 then
      Lwt.return (`String (Buffer.sub t.buf pos' len))
    else
      Lwt_stream.nget (abs len') t.stream
      >>= fun chars ->
      if List.length chars = len' then (
        String.of_seq (List.to_seq chars) |> Buffer.add_string t.buf;
        Lwt.return (`String (Buffer.sub t.buf pos' len))
      ) else
        Lwt.return `Eof

  let commit t ~pos =
    Buffer.reset t.buf;
    t.committed <- pos;
    Lwt.return_unit
end)