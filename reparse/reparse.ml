module P = Parser

module Parser = P
[@@deprecated
  "[since v3.0.0] Use Reparse. All functions from Parser have been migrated to Reparse \
   module "]

include P
include P.Let_syntax
