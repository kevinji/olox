open! Core
open! Async
open! Import

module Type : sig
  type t =
    (* Single-character tokens. *)
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    (* One or two character tokens. *)
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    (* Literals. *)
    | IDENTIFIER of string
    | STRING of string
    | NUMBER of float
    (* Keywords. *)
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF
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
