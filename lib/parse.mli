(*-------------------------------------------------------------------------
 * Copyright (c) 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

module Make : functor (Source : Source.S) ->
  Parse_sig.S with type src = Source.t

module String_parser : Parse_sig.S with type src = Source.String.t
module File_parser : Parse_sig.S with type src = Source.File.t
