open! Core
open! Async
open! Import

type expression =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping
[@@deriving sexp_of]

and literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
[@@deriving sexp_of]

and grouping = Group of expression [@@deriving sexp_of]

and unary_symbol =
  | Negative
  | Not
[@@deriving sexp_of]

and unary =
  | Negative of expression
  | Not of expression
[@@deriving sexp_of]

and binary = Infix of expression * operator * expression [@@deriving sexp_of]

and operator =
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Plus
  | Minus
  | Multiply
  | Divide
  | Concat
[@@deriving sexp_of]
