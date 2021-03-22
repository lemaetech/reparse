include module type of Parser

include module type of Parser.Let_syntax

(**/**)

(** @deprecated [since v3.0.0] Use Reparse. All functions from Parser have been
    migrated to [Reparse] module *)
module Parser = Parser
[@@deprecated
  "[since v3.0.0] Use Reparse. All functions from Parser have been migrated to \
   Reparse module "]
