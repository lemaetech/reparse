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
  type stream = {
    stream : char Lwt_stream.t;
    mutable buf : Cstruct.t;
    mutable last_trimmed_pos : int;
        (* An input position marker. The marker restricts the parser from
           backtracking beyound this point. Any attempt to do so will raise an
           exception. *)
  }

  module Promise = struct
    type 'a t = 'a Lwt.t

    let return = Lwt.return

    let bind f p = Lwt.bind p f

    let catch = Lwt.catch
  end

  module Input = struct
    open Lwt.Infix

    type t = stream

    type 'a promise = 'a Lwt.t

    let trim_buffer t ~pos =
      let pos' = pos - t.last_trimmed_pos in
      let bytes_to_copy = Cstruct.length t.buf - pos' in
      let buf =
        if bytes_to_copy <= 0 then Cstruct.empty
        else Cstruct.sub t.buf pos' bytes_to_copy
      in
      t.buf <- buf;
      t.last_trimmed_pos <- pos;
      Lwt.return ()

    let buffer_pos_len t ~pos ~len =
      let pos' = pos - t.last_trimmed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      (pos', len')

    let get_char_common t ~pos =
      let pos', len' = buffer_pos_len t ~pos ~len:1 in
      if len' >= 0 then Lwt.return (`Return (Cstruct.get_char t.buf pos'))
      else
        Lwt_stream.get t.stream >|= function
        | Some c -> `Additional_byte_read c
        | None -> `Eof

    let get_char t ~pos =
      get_char_common t ~pos >|= function
      | `Return c -> `Char c
      | `Additional_byte_read c ->
          let new_buf = Cstruct.create_unsafe (Cstruct.length t.buf + 1) in
          Cstruct.blit t.buf 0 new_buf 0 (Cstruct.length t.buf);
          Cstruct.set_char new_buf (Cstruct.length new_buf - 1) c;
          t.buf <- new_buf;
          `Char c
      | `Eof -> `Eof

    let get_char_unbuffered t ~pos =
      get_char_common t ~pos >|= function
      | `Return c -> `Char c
      | `Additional_byte_read c -> `Char c
      | `Eof -> `Eof

    let get_cstruct t ~pos ~len =
      let pos', len' = buffer_pos_len t ~pos ~len in
      if len' >= 0 then Lwt.return (`Cstruct (Cstruct.sub t.buf pos' len))
      else
        let len' = abs len' in
        Lwt_stream.nget len' t.stream >|= fun chars ->
        let len'' = List.length chars in
        if len'' > 0 then (
          chars |> List.to_seq |> String.of_seq |> Cstruct.of_string |> fun b ->
          let new_buf = Cstruct.append t.buf b in
          let len' = Cstruct.length new_buf - pos' in
          let len = if len' < len then len' else len in
          t.buf <- new_buf;
          `Cstruct (Cstruct.sub t.buf pos' len))
        else `Eof

    let last_trimmed_pos t = Lwt.return t.last_trimmed_pos

    let buffer_size t = Lwt.return @@ Some (Cstruct.length t.buf)
  end

  include Reparse.Make (Promise) (Input)

  let input_of_stream stream =
    { stream; buf = Cstruct.empty; last_trimmed_pos = 0 }
end
