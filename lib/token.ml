open Core

module Type = struct
  type _ t =
    (* Single-character tokens. *)
    | LEFT_PAREN : unit t
    | RIGHT_PAREN : unit t
    | LEFT_BRACE : unit t
    | RIGHT_BRACE : unit t
    | COMMA : unit t
    | DOT : unit t
    | MINUS : unit t
    | PLUS : unit t
    | SEMICOLON : unit t
    | SLASH : unit t
    | STAR : unit t
    (* One or two character tokens. *)
    | BANG : unit t
    | BANG_EQUAL : unit t
    | EQUAL : unit t
    | EQUAL_EQUAL : unit t
    | GREATER : unit t
    | GREATER_EQUAL : unit t
    | LESS : unit t
    | LESS_EQUAL : unit t
    (* Literals. *)
    | IDENTIFIER : string -> string t
    | STRING : string -> string t
    | NUMBER : float -> float t
    (* Keywords. *)
    | AND : unit t
    | CLASS : unit t
    | ELSE : unit t
    | FALSE : unit t
    | FUN : unit t
    | FOR : unit t
    | IF : unit t
    | NIL : unit t
    | OR : unit t
    | PRINT : unit t
    | RETURN : unit t
    | SUPER : unit t
    | THIS : unit t
    | TRUE : unit t
    | VAR : unit t
    | WHILE : unit t
    | EOF : unit t

  let name (type a) (t : a t) =
    match t with
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

  let literal (type a) (t : a t) =
    match t with
    | IDENTIFIER identifier -> identifier
    | STRING string -> string
    | NUMBER number -> Float.to_string number
    | LEFT_PAREN -> ""
    | RIGHT_PAREN -> ""
    | LEFT_BRACE -> ""
    | RIGHT_BRACE -> ""
    | COMMA -> ""
    | DOT -> ""
    | MINUS -> ""
    | PLUS -> ""
    | SEMICOLON -> ""
    | SLASH -> ""
    | STAR -> ""
    | BANG -> ""
    | BANG_EQUAL -> ""
    | EQUAL -> ""
    | EQUAL_EQUAL -> ""
    | GREATER -> ""
    | GREATER_EQUAL -> ""
    | LESS -> ""
    | LESS_EQUAL -> ""
    | AND -> ""
    | CLASS -> ""
    | ELSE -> ""
    | FALSE -> ""
    | FUN -> ""
    | FOR -> ""
    | IF -> ""
    | NIL -> ""
    | OR -> ""
    | PRINT -> ""
    | RETURN -> ""
    | SUPER -> ""
    | THIS -> ""
    | TRUE -> ""
    | VAR -> ""
    | WHILE -> ""
    | EOF -> ""
  ;;
end

type 'a t =
  { type_ : 'a Type.t
  ; lexeme : string
  ; line : int
  }

let to_string t = sprintf "%s %s %s" (Type.name t.type_) t.lexeme (Type.literal t.type_)
