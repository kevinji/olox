open! Core
open! Async
open! Import

module Type = struct
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

  include (val Comparator.make ~compare ~sexp_of_t)

  let equal = [%compare.equal: t]

  let name = function
    | Left_paren -> "LEFT_PAREN"
    | Right_paren -> "RIGHT_PAREN"
    | Left_brace -> "LEFT_BRACE"
    | Right_brace -> "RIGHT_BRACE"
    | Comma -> "COMMA"
    | Dot -> "DOT"
    | Minus -> "MINUS"
    | Plus -> "PLUS"
    | Semicolon -> "SEMICOLON"
    | Slash -> "SLASH"
    | Star -> "STAR"
    | Bang -> "BANG"
    | Bang_equal -> "BANG_EQUAL"
    | Equal -> "EQUAL"
    | Equal_equal -> "EQUAL_EQUAL"
    | Greater -> "GREATER"
    | Greater_equal -> "GREATER_EQUAL"
    | Less -> "LESS"
    | Less_equal -> "LESS_EQUAL"
    | Identifier (_ : string) -> "IDENTIFIER"
    | String (_ : string) -> "STRING"
    | Number (_ : float) -> "NUMBER"
    | And -> "AND"
    | Class -> "CLASS"
    | Else -> "ELSE"
    | False -> "FALSE"
    | Fun -> "FUN"
    | For -> "FOR"
    | If -> "IF"
    | Nil -> "NIL"
    | Or -> "OR"
    | Print -> "PRINT"
    | Return -> "RETURN"
    | Super -> "SUPER"
    | This -> "THIS"
    | True -> "TRUE"
    | Var -> "VAR"
    | While -> "WHILE"
    | Eof -> "EOF"
  ;;

  let literal = function
    | Identifier str | String str -> Some str
    | Number float -> Some (Float.to_string float)
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
    | Bang
    | Bang_equal
    | Equal
    | Equal_equal
    | Greater
    | Greater_equal
    | Less
    | Less_equal
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
    | Eof -> None
  ;;
end

type t =
  { type_ : Type.t
  ; lexeme : string
  ; line : int
  }
[@@deriving sexp_of]

let to_string t =
  [ Some (Type.name t.type_); Some t.lexeme; Type.literal t.type_ ]
  |> List.filter_opt
  |> String.concat ~sep:" "
;;
