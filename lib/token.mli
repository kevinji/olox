open! Core
open! Async
open! Import

module Type : sig
  type t =
    (* Single-character tokens. *)
    | Left_paren
    | Right_paren
    | Left_brace
    | Right_brace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    (* One or two character tokens. *)
    | Bang
    | Bang_equal
    | Equal
    | Equal_equal
    | Greater
    | Greater_equal
    | Less
    | Less_equal
    (* Literals. *)
    | Identifier of string
    | String of string
    | Number of float
    (* Keywords. *)
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Eof
  [@@deriving compare, sexp_of]

  include Comparator.S with type t := t

  val equal : t -> t -> bool
  val name : t -> string
end

type t =
  { type_ : Type.t
  ; lexeme : string
  ; line : int
  }
[@@deriving sexp_of]

val to_string : t -> string
