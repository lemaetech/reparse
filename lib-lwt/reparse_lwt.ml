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
    ; mutable last_trimmed_pos : int
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

    let pos_err pos fn =
      invalid_arg @@ Format.sprintf "invalid arg [pos: %d] in [%s]" pos fn

    let trim_buffer t ~pos =
      if pos < 0 || pos < t.last_trimmed_pos then pos_err pos "trim_buffer";

      let bytes_to_trim = pos - t.last_trimmed_pos in
      let new_buf_sz = Cstruct.length t.buf - bytes_to_trim in
      let buf =
        if new_buf_sz <= 0 then
          Cstruct.empty
        else
          let buf = Cstruct.create new_buf_sz in
          Cstruct.blit t.buf pos buf 0 bytes_to_trim;
          buf
      in
      t.buf <- buf;
      t.last_trimmed_pos <- pos;
      Lwt.return_unit

    let get_char t ~pos =
      if pos < 0 || pos < t.last_trimmed_pos then pos_err pos "get";

      let len = 1 in
      let pos' = pos - t.last_trimmed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      if len' >= 0 then
        Lwt.return (`Char (Cstruct.get_char t.buf pos'))
      else
        Lwt_stream.get t.stream
        >|= function
        | Some c ->
          let s1 = Cstruct.create_unsafe (Cstruct.length t.buf + 1) in
          Cstruct.blit t.buf 0 s1 0 (Cstruct.length t.buf);
          Cstruct.set_char s1 (Cstruct.length s1 - 1) c;
          t.buf <- s1;
          `Char c
        | None -> `Eof

    let get_cstruct_unbuffered t ~pos ~len =
      if len < 0 then raise (invalid_arg "len");
      if pos < 0 || pos < t.last_trimmed_pos then pos_err pos "get_unbuffered";

      let pos' = pos - t.last_trimmed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      if len' >= 0 then
        Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))
      else
        Lwt_stream.nget (abs len') t.stream
        >|= fun chars ->
        let s1 =
          chars
          |> List.to_seq
          |> String.of_seq
          |> Cstruct.of_string
          |> Cstruct.append (Cstruct.sub t.buf pos' len)
        in
        `Cstruct s1

    let get_cstruct t ~pos ~len =
      if len < 0 then raise (invalid_arg "len");
      if pos < 0 || pos < t.last_trimmed_pos then pos_err pos "get";

      let pos' = pos - t.last_trimmed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      if len' >= 0 then
        Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))
      else
        Lwt_stream.nget (abs len') t.stream
        >|= fun chars ->
        let s1 =
          chars
          |> List.to_seq
          |> String.of_seq
          |> Cstruct.of_string
          |> Cstruct.append t.buf
        in
        t.buf <- s1;
        `Cstruct (Cstruct.sub t.buf pos' len)

    let last_trimmed_pos t = return t.last_trimmed_pos
  end)

  let input_of_stream stream =
    { stream; buf = Cstruct.empty; last_trimmed_pos = 0 }
end
