open Core
include Core_kernel.No_polymorphic_compare

let fail ~line message = printf "[line %d] Error: %s\n" line message
