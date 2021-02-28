include module type of Parser
include module type of Parser.Let_syntax

(**/**)

module Parser = Parser
[@@deprecated
  "[since v3.0.0] Use Reparse. All functions from Parser have been migrated to Reparse \
   module "]
(** @deprecated [since v3.0.0] Use Reparse. All functions from Parser have been migrated
    to [Reparse] module *)
