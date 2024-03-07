open! Core
open! Async
open! Import

type t =
  { tokens : Token.t Fqueue.t
  ; line : int
  ; has_error : bool
  }

let create () = { tokens = Fqueue.empty; line = 1; has_error = false }
let advance_line t = { t with line = t.line + 1 }

module Tokenizer = struct
  let token type_ lexeme t =
    let token = { Token.type_; lexeme; line = t.line } in
    { t with tokens = Fqueue.enqueue t.tokens token }
  ;;

  let token_ch type_ lexeme = token type_ (Char.to_string lexeme)
  let is_newline = Char.( = ) '\n'
  let whitespace_chars = Set.of_list (module Char) [ ' '; '\r'; '\t' ]

  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  ;;

  let keyword_name token_type = Token.Type.name token_type |> String.lowercase

  let keyword_map =
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
    |> List.map ~f:(fun token_type -> keyword_name token_type, token_type)
    |> Map.of_alist_exn (module String)
  ;;

  (* Tokens. *)
  open Angstrom

  let eof = end_of_input *> return (token EOF "") <?> "EOF"
  let left_paren = char '(' >>| token_ch LEFT_PAREN <?> "LEFT_PAREN"
  let right_paren = char ')' >>| token_ch RIGHT_PAREN <?> "RIGHT_PAREN"
  let left_brace = char '{' >>| token_ch LEFT_BRACE <?> "LEFT_BRACE"
  let right_brace = char '}' >>| token_ch RIGHT_BRACE <?> "RIGHT_BRACE"
  let comma = char ',' >>| token_ch COMMA <?> "COMMA"
  let dot = char '.' >>| token_ch DOT <?> "DOT"
  let minus = char '-' >>| token_ch MINUS <?> "MINUS"
  let plus = char '+' >>| token_ch PLUS <?> "PLUS"
  let semicolon = char ';' >>| token_ch SEMICOLON <?> "SEMICOLON"
  let star = char '*' >>| token_ch STAR <?> "STAR"
  let bang_equal = string "!=" >>| token BANG_EQUAL <?> "BANG_EQUAL"
  let bang = char '!' >>| token_ch BANG <?> "BANG"
  let equal_equal = string "==" >>| token EQUAL_EQUAL <?> "EQUAL_EQUAL"
  let equal = char '=' >>| token_ch EQUAL <?> "EQUAL"
  let less_equal = string "<=" >>| token LESS_EQUAL <?> "LESS_EQUAL"
  let less = char '<' >>| token_ch LESS <?> "LESS"
  let greater_equal = string ">=" >>| token GREATER_EQUAL <?> "GREATER_EQUAL"
  let greater = char '>' >>| token_ch GREATER <?> "GREATER"

  let comment =
    string "//" *> skip_while (Fn.non is_newline) *> return Fn.id <?> "COMMENT"
  ;;

  let slash = char '/' >>| token_ch SLASH <?> "SLASH"
  let newline = char '\n' *> return advance_line <?> "NEWLINE"
  let whitespace = take_while1 (Set.mem whitespace_chars) *> return Fn.id <?> "WHITESPACE"

  let string_ =
    char '"' *> take_while (Char.( <> ) '"')
    <* char '"'
    >>| (fun str t ->
          let t =
            String.fold str ~init:t ~f:(fun t ch ->
              if is_newline ch then advance_line t else t)
          in
          token (STRING str) (sprintf "\"%s\"" str) t)
    <?> "STRING"
  ;;

  let int = take_while1 Char.is_digit

  let number =
    int
    >>= fun int_part ->
    char '.' *> int
    >>| sprintf "%s.%s" int_part
    <|> return int_part
    >>| (fun number -> token (NUMBER (Float.of_string number)) number)
    <?> "NUMBER"
  ;;

  let keyword_or_identifier =
    take_while1 is_alphanum
    >>| (fun identifier ->
          match Map.find keyword_map identifier with
          | Some keyword_type -> token keyword_type identifier
          | None -> token (IDENTIFIER identifier) identifier)
    <?> "KEYWORD / IDENTIFIER"
  ;;

  let error =
    any_char
    >>| (fun ch t ->
          print_err ~line:t.line (sprintf "Unexpected character %c" ch);
          { t with has_error = true })
    <?> "ERROR"
  ;;

  let program =
    fix (fun p ->
      at_end_of_input
      >>= function
      | true -> eof
      | false ->
        choice
          ~failure_msg:"BUG: All errors should be caught by the error parser."
          [ left_paren
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
          ; keyword_or_identifier
          ; error
          ]
        >>| Fn.(flip compose)
        <*> p)
    <?> "PROGRAM"
  ;;
end

let parse ~source =
  let open Or_error.Let_syntax in
  let t = create () in
  let%bind t_func =
    Angstrom.parse_string ~consume:All Tokenizer.program source
    |> Result.map_error ~f:Error.of_string
  in
  let t = t_func t in
  let%map () =
    if t.has_error
    then Or_error.error_string "Program had errors, and could not be fully parsed."
    else return ()
  in
  Fqueue.to_list t.tokens
;;
