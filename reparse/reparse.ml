(** Convenience module. When one does [open Reparse] all of [Parser] modules are
    available. Additionally [ppx_let] syntax support is also available. *)

module Parser = Parser
include Parser
include Parser.Let_syntax
