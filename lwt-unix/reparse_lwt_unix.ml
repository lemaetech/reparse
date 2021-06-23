(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

open Lwt.Infix

module Promise = struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind f p = Lwt.bind p f
  let catch = Lwt.catch
end

module Fd = struct
  module Input =
    Reparse.Make_buffered_input
      (Promise)
      (struct
        type t = Lwt_unix.file_descr
        type 'a promise = 'a Lwt.t

        let read t ~len =
          let buf = Cstruct.create len in
          Lwt_bytes.read t buf.buffer 0 len
          >|= fun read_count ->
          if read_count <= 0 then `Eof
          else if read_count < len then `Cstruct (Cstruct.sub buf 0 read_count)
          else `Cstruct buf
      end)

  include Reparse.Make (Promise) (Input)

  let create_input file_descr = Input.create file_descr
end

module Channel = struct
  module Input =
    Reparse.Make_buffered_input
      (Promise)
      (struct
        type t = Lwt_io.input_channel
        type 'a promise = 'a Lwt.t

        let read t ~len =
          let buf = Cstruct.create len in
          Lwt_io.read_into_bigstring t buf.buffer 0 len
          >|= fun read_count ->
          if read_count <= 0 then `Eof
          else if read_count < len then `Cstruct (Cstruct.sub buf 0 read_count)
          else `Cstruct buf
      end)

  include Reparse.Make (Promise) (Input)

  let create_input channel = Input.create channel
end
