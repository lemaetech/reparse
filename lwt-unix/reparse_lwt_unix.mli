(*-------------------------------------------------------------------------
 * Copyright (c) 2020, 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** [Fd] is a [Lwt_unix.file_descr] input based [Reparse] parser. *)
module Fd : sig
  include Reparse.PARSER with type 'a promise = 'a Lwt.t

  val create_input : Lwt_unix.file_descr -> input
end

(** [Channel] is a [Lwt_io.input_channel] input based [Reparse] parser. *)
module Channel : sig
  include Reparse.PARSER with type 'a promise = 'a Lwt.t

  val create_input : Lwt_io.input_channel -> input
end
