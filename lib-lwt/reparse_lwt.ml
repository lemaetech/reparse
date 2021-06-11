(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

module Stream = struct
  type stream =
    { stream : char Lwt_stream.t
    ; mutable buf : Cstruct.t
    ; mutable committed_pos : int
          (* An input position marker. The marker restricts the parser from
             backtracking beyound this point. Any attempt to do so will raise an
             exception. *)
    }

  include Reparse.Make (struct
    open Lwt.Infix

    type 'a promise = 'a Lwt.t

    type t = stream

    let return = Lwt.return

    let bind f p = Lwt.bind p f

    let commit t ~pos =
      t.buf <- Cstruct.empty;
      t.committed_pos <- pos;
      Lwt.return_unit

    let get_unbuffered t ~pos ~len =
      if len < 0 then raise (invalid_arg "len");
      if pos < 0 || pos < t.committed_pos then
        invalid_arg (Format.sprintf "pos: %d" pos);

      let pos' = pos - t.committed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      if len' >= 0 then
        Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))
      else
        Lwt_stream.nget (abs len') t.stream
        >>= fun chars ->
        let s1 =
          chars
          |> List.to_seq
          |> String.of_seq
          |> Cstruct.of_string
          |> Cstruct.append (Cstruct.sub t.buf pos' len)
        in
        Lwt.return (`Cstruct s1)

    let get t ~pos ~len =
      if len < 0 then raise (invalid_arg "len");
      if pos < 0 || pos < t.committed_pos then
        invalid_arg (Format.sprintf "pos: %d" pos);

      let pos' = pos - t.committed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      if len' >= 0 then
        Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))
      else
        Lwt_stream.nget (abs len') t.stream
        >>= fun chars ->
        let s1 =
          chars
          |> List.to_seq
          |> String.of_seq
          |> Cstruct.of_string
          |> Cstruct.append t.buf
        in
        t.buf <- s1;
        Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))

    let committed_pos t = return t.committed_pos
  end)

  let input_of_stream stream =
    { stream; buf = Cstruct.empty; committed_pos = 0 }
end
