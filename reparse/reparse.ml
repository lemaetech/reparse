module P = Parser
include P
include P.Let_syntax

module Parser = P
[@@deprecated
  "[since v3.0.0] Use Reparse. All functions from Parser have been migrated to Reparse \
   module "]
(** @deprecated [since v3.0.0] Use Reparse. All functions from Parser have been migrated
    to [Reparse] module *)
