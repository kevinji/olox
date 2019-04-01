open Core

module Type = struct
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

  let name = function
    | LEFT_PAREN -> "LEFT_PAREN"
    | RIGHT_PAREN -> "RIGHT_PAREN"
    | LEFT_BRACE -> "LEFT_BRACE"
    | RIGHT_BRACE -> "RIGHT_BRACE"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | MINUS -> "MINUS"
    | PLUS -> "PLUS"
    | SEMICOLON -> "SEMICOLON"
    | SLASH -> "SLASH"
    | STAR -> "STAR"
    | BANG -> "BANG"
    | BANG_EQUAL -> "BANG_EQUAL"
    | EQUAL -> "EQUAL"
    | EQUAL_EQUAL -> "EQUAL_EQUAL"
    | GREATER -> "GREATER"
    | GREATER_EQUAL -> "GREATER_EQUAL"
    | LESS -> "LESS"
    | LESS_EQUAL -> "LESS_EQUAL"
    | IDENTIFIER (_ : string) -> "IDENTIFIER"
    | STRING (_ : string) -> "STRING"
    | NUMBER (_ : float) -> "NUMBER"
    | AND -> "AND"
    | CLASS -> "CLASS"
    | ELSE -> "ELSE"
    | FALSE -> "FALSE"
    | FUN -> "FUN"
    | FOR -> "FOR"
    | IF -> "IF"
    | NIL -> "NIL"
    | OR -> "OR"
    | PRINT -> "PRINT"
    | RETURN -> "RETURN"
    | SUPER -> "SUPER"
    | THIS -> "THIS"
    | TRUE -> "TRUE"
    | VAR -> "VAR"
    | WHILE -> "WHILE"
    | EOF -> "EOF"
  ;;

  let literal = function
    | IDENTIFIER str | STRING str -> str
    | NUMBER float -> Float.to_string float
    | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | COMMA | DOT | MINUS | PLUS
    | SEMICOLON | SLASH | STAR | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL | GREATER
    | GREATER_EQUAL | LESS | LESS_EQUAL | AND | CLASS | ELSE | FALSE | FUN | FOR | IF
    | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE | EOF ->
      ""
  ;;
end

type t =
  { type_ : Type.t
  ; lexeme : string
  ; line : int
  }

let to_string t = sprintf "%s %s %s" (Type.name t.type_) t.lexeme (Type.literal t.type_)
