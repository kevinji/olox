open! Core
open! Async

let print_err ~line ?(where = "") message =
  eprintf "[line %d] Error%s: %s\n" line where message
;;
