(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

type stream =
  { stream : char Lwt_stream.t
  ; buf : Buffer.t
  ; mutable committed_pos : int
        (* An input position marker. The marker restricts the parser from
           backtracking beyound this point. Any attempt to do so will raise an
           exception. *)
  }

let create_stream stream = { stream; buf = Buffer.create 0; committed_pos = 0 }

module Stream = Reparse.Make (struct
  open Lwt.Infix

  type 'a promise = 'a Lwt.t

  type t = stream

  let return = Lwt.return

  let bind f p = Lwt.bind p f

  let commit t ~pos =
    Buffer.reset t.buf;
    t.committed_pos <- pos;
    Lwt.return_unit

  let get_unbuffered t ~pos ~len =
    if len < 0 then raise (invalid_arg "len");
    if pos < 0 || pos < t.committed_pos then
      invalid_arg (Format.sprintf "pos: %d" pos);

    let pos' = pos - t.committed_pos in
    let len' = Buffer.length t.buf - (pos' + len) in
    if len' >= 0 then
      Lwt.return (`String (Buffer.sub t.buf pos' len))
    else
      Lwt_stream.nget (abs len') t.stream
      >>= fun chars ->
      let s = String.of_seq (List.to_seq chars) in
      Lwt.return (`String s)

  let get t ~pos ~len =
    if len < 0 then raise (invalid_arg "len");
    if pos < 0 || pos < t.committed_pos then
      invalid_arg (Format.sprintf "pos: %d" pos);

    let pos' = pos - t.committed_pos in
    let len' = Buffer.length t.buf - (pos' + len) in
    if len' >= 0 then
      Lwt.return (`String (Buffer.sub t.buf pos' len))
    else
      Lwt_stream.nget (abs len') t.stream
      >>= fun chars ->
      String.of_seq (List.to_seq chars) |> Buffer.add_string t.buf;
      Lwt.return (`String (Buffer.sub t.buf pos' len))

  let committed_pos t = return t.committed_pos
end)
