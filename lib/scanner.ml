open Core
open! Import

type t =
  { tokens : Token.t Fqueue.t
  ; line : int
  }

let create () = { tokens = Fqueue.empty; line = 1 }
let advance_line t = { t with line = t.line + 1 }

module Tokenizer = struct
  let token type_ lexeme t =
    let token = { Token.type_; lexeme; line = t.line } in
    { t with tokens = Fqueue.enqueue t.tokens token }
  ;;

  let token_ch type_ lexeme = token type_ (Char.to_string lexeme)
  let is_newline = Char.( = ) '\n'
  let whitespace_chars = Char.Set.of_list [ ' '; '\r'; '\t' ]

  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  ;;

  let keyword_tokens =
    [ Token.Type.AND
    ; CLASS
    ; ELSE
    ; FALSE
    ; FUN
    ; FOR
    ; IF
    ; NIL
    ; OR
    ; PRINT
    ; RETURN
    ; SUPER
    ; THIS
    ; TRUE
    ; VAR
    ; WHILE
    ; EOF
    ]
  ;;

  let keyword_name token_type = Token.Type.name token_type |> String.lowercase

  (* Tokens. *)
  open Angstrom

  let eof = end_of_input *> return (token EOF "")
  let left_paren = char '(' >>| token_ch LEFT_PAREN
  let right_paren = char ')' >>| token_ch RIGHT_PAREN
  let left_brace = char '{' >>| token_ch LEFT_BRACE
  let right_brace = char '}' >>| token_ch RIGHT_BRACE
  let comma = char ',' >>| token_ch COMMA
  let dot = char '.' >>| token_ch DOT
  let minus = char '-' >>| token_ch MINUS
  let plus = char '+' >>| token_ch PLUS
  let semicolon = char ';' >>| token_ch SEMICOLON
  let star = char '*' >>| token_ch STAR
  let bang_equal = string "!=" >>| token BANG_EQUAL
  let bang = char '!' >>| token_ch BANG
  let equal_equal = string "==" >>| token EQUAL_EQUAL
  let equal = char '=' >>| token_ch EQUAL
  let less_equal = string "<=" >>| token LESS_EQUAL
  let less = char '<' >>| token_ch LESS
  let greater_equal = string ">=" >>| token GREATER_EQUAL
  let greater = char '>' >>| token_ch GREATER
  let comment = string "//" *> skip_while (Fn.non is_newline) *> return Fn.id
  let slash = char '/' >>| token_ch SLASH
  let newline = char '\n' *> return advance_line
  let whitespace = take_while1 (Set.mem whitespace_chars) *> return Fn.id

  let string_ =
    char '"' *> take_while (Char.( <> ) '"')
    <* char '"'
    >>| fun str t ->
    let t =
      String.fold str ~init:t ~f:(fun t ch -> if is_newline ch then advance_line t else t)
    in
    token (STRING str) (sprintf "\"%s\"" str) t
  ;;

  let int = take_while1 Char.is_digit

  let number =
    int
    >>= fun int_part ->
    char '.' *> int
    >>| sprintf "%s.%s" int_part
    <|> return int_part
    >>| fun number -> token (NUMBER (Float.of_string number)) number
  ;;

  let keyword =
    List.map keyword_tokens ~f:(fun token_type ->
        let keyword = keyword_name token_type in
        string keyword
        <* (peek_char
           >>= function
           | None -> return ()
           | Some ch -> if is_alphanum ch then fail "Not a keyword." else return ())
        >>| token token_type)
    |> choice ~failure_msg:"Not a keyword."
  ;;

  let identifier =
    take_while1 is_alphanum
    >>| fun identifier -> token (IDENTIFIER identifier) identifier
  ;;

  let program =
    fix (fun p ->
        at_end_of_input
        >>= function
        | true -> return Fn.id
        | false ->
          choice
            ~failure_msg:"Unexpected character."
            [ eof
            ; left_paren
            ; right_paren
            ; left_brace
            ; right_brace
            ; comma
            ; dot
            ; minus
            ; plus
            ; semicolon
            ; star
            ; bang_equal
            ; bang
            ; equal_equal
            ; equal
            ; less_equal
            ; less
            ; greater_equal
            ; greater
            ; comment
            ; slash
            ; newline
            ; whitespace
            ; string_
            ; number
            ; keyword
            ; identifier
            ]
          >>| Fn.(flip compose)
          <*> p)
  ;;
end

let parse ~source =
  let open Or_error.Let_syntax in
  let t = create () in
  let%map t_func =
    Angstrom.parse_string Tokenizer.program source |> Result.map_error ~f:Error.of_string
  in
  let t = t_func t in
  Fqueue.to_list t.tokens
;;
