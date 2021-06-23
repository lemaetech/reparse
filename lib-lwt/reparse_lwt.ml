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
  module Promise = struct
    type 'a t = 'a Lwt.t

    let return = Lwt.return

    let bind f p = Lwt.bind p f

    let catch = Lwt.catch
  end

  module Input =
    Reparse.Make_buffered
      (Promise)
      (struct
        type t = char Lwt_stream.t

        type 'a promise = 'a Lwt.t

        let read t ~len =
          let open Lwt.Infix in
          Lwt_stream.nget len t >|= fun chars ->
          let len'' = List.length chars in
          if len'' > 0 then
            `Cstruct (chars |> List.to_seq |> String.of_seq |> Cstruct.of_string)
          else `Eof
      end)

  include Reparse.Make (Promise) (Input)

  let input_of_stream stream = Input.create stream
end
