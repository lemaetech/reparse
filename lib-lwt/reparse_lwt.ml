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

    let trim_buffer t ~pos =
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
      return ()

    let buffer_pos_len t ~pos ~len =
      let pos' = pos - t.last_trimmed_pos in
      let len' = Cstruct.length t.buf - (pos' + len) in
      (pos', len')

    let get_char_common t ~pos =
      let pos', len' = buffer_pos_len t ~pos ~len:1 in
      if len' >= 0 then
        return (`Return (Cstruct.get_char t.buf pos'))
      else
        Lwt_stream.get t.stream
        >|= function
        | Some c -> `Additional_byte_read c
        | None -> `Eof

    let get_char t ~pos =
      get_char_common t ~pos
      >|= function
      | `Return c -> `Char c
      | `Additional_byte_read c ->
        let new_buf = Cstruct.create_unsafe (Cstruct.length t.buf + 1) in
        Cstruct.blit t.buf 0 new_buf 0 (Cstruct.length t.buf);
        Cstruct.set_char new_buf (Cstruct.length new_buf - 1) c;
        t.buf <- new_buf;
        `Char c
      | `Eof -> `Eof

    let get_char_unbuffered t ~pos =
      get_char_common t ~pos
      >|= function
      | `Return c -> `Char c
      | `Additional_byte_read c -> `Char c
      | `Eof -> `Eof

    let get_cstruct_common t ~pos ~len =
      let pos', len' = buffer_pos_len t ~pos ~len in
      if len' >= 0 then
        return (`Return (Cstruct.sub t.buf pos' len))
      else
        let len' = abs len' in
        Lwt_stream.nget len' t.stream
        >|= fun chars ->
        let len'' = List.length chars in
        if len'' > 0 then
          chars
          |> List.to_seq
          |> String.of_seq
          |> Cstruct.of_string
          |> fun bytes -> `Additional_bytes_read (bytes, pos')
        else
          `Eof

    let get_cstruct_unbuffered t ~pos ~len =
      get_cstruct_common t ~pos ~len
      >|= function
      | `Eof -> `Eof
      | `Return bytes -> `Cstruct bytes
      | `Additional_bytes_read (additional_bytes, pos') ->
        let b1 =
          let len' = Cstruct.length t.buf - pos' in
          Cstruct.sub t.buf pos' len'
        in
        let bytes = Cstruct.(append b1 additional_bytes) in
        `Cstruct bytes

    let get_cstruct t ~pos ~len =
      get_cstruct_common t ~pos ~len
      >|= function
      | `Eof -> `Eof
      | `Return buf -> `Cstruct buf
      | `Additional_bytes_read (additional_bytes, pos') ->
        let new_buf = Cstruct.append t.buf additional_bytes in
        let len' = Cstruct.length new_buf - pos' in
        let len =
          if len' < len then
            len'
          else
            len
        in
        t.buf <- new_buf;
        `Cstruct (Cstruct.sub t.buf pos' len)

    let last_trimmed_pos t = return t.last_trimmed_pos

    let buffer_size t = return @@ Some (Cstruct.length t.buf)
  end)

  let input_of_stream stream =
    { stream; buf = Cstruct.empty; last_trimmed_pos = 0 }
end

