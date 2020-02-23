open! Core
open! Async
open! Import

type _ expression =
  | Literal : 'a literal -> 'a expression
  | Unary : 'a unary -> 'a expression
  | Binary : 'a binary -> 'a expression
  | Grouping : 'a grouping -> 'a expression

and _ literal =
  | Number : float -> float literal
  | String : string -> string literal
  | Bool : bool -> bool literal
  | Nil

and _ grouping = Group : 'a expression -> 'a grouping

and _ unary =
  | Negative : float expression -> float unary
  | Not : bool expression -> bool unary

and _ binary =
  | Infix : 'expr expression * ('expr, 'out) operator * 'expr expression -> 'out binary

and (_, _) operator =
  | Equal : ('a, bool) operator
  | Not_equal : ('a, bool) operator
  | Less : (float, bool) operator
  | Less_equal : (float, bool) operator
  | Greater : (float, bool) operator
  | Greater_equal : (float, bool) operator
  | Plus : (float, float) operator
  | Minus : (float, float) operator
  | Multiply : (float, float) operator
  | Divide : (float, float) operator
  | Concat : (string, string) operator
[@@deriving sexp_of]
