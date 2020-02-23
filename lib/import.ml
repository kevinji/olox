open! Core
open! Async

let print_err ~line message = printf "[line %d] Error: %s\n" line message
