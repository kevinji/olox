open! Core
open! Async
open! Import

let print_token_err ?token message =
  let line =
    match token with
    | None -> 0
    | Some token -> token.Token.line
  in
  let where =
    match token with
    | None -> ""
    | Some token ->
      (match token.type_ with
       | Eof -> " at end"
       | _ -> sprintf " at '%s'" token.lexeme)
  in
  print_err ~line ~where message
;;

let match_token (tokens : Token.t list) ~f =
  match tokens with
  | [] -> None
  | token :: tokens ->
    (match f token.type_ with
     | None -> None
     | Some expr -> Some (expr, tokens))
;;

let rec parse_expression tokens = parse_equality tokens

and parse_equality tokens =
  let expr, tokens = parse_comparison tokens in
  let rec helper expr tokens =
    match
      match_token tokens ~f:(function
        | Equal -> Some Expr.Equal
        | Bang_equal -> Some Expr.Not_equal
        | _ -> None)
    with
    | None -> expr, tokens
    | Some (op, tokens) ->
      let right, tokens = parse_comparison tokens in
      let expr = Expr.Binary (Infix (expr, op, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_comparison tokens : Expr.expression * Token.t list =
  let expr, tokens = parse_addition tokens in
  match
    match_token tokens ~f:(function
      | Greater -> Some Expr.Greater
      | Greater_equal -> Some Expr.Greater_equal
      | Less -> Some Expr.Less
      | Less_equal -> Some Expr.Less_equal
      | _ -> None)
  with
  | None -> expr, tokens
  | Some (op, tokens) ->
    let right, tokens = parse_addition tokens in
    Expr.Binary (Infix (expr, op, right)), tokens

and parse_addition tokens : Expr.expression * Token.t list =
  let expr, tokens = parse_multiplication tokens in
  let rec helper expr tokens =
    match
      match_token tokens ~f:(function
        | Minus -> Some Expr.Minus
        | Plus -> Some Expr.Plus
        | _ -> None)
    with
    | None -> expr, tokens
    | Some (op, tokens) ->
      let right, tokens = parse_multiplication tokens in
      let expr = Expr.Binary (Infix (expr, op, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_multiplication tokens : Expr.expression * Token.t list =
  let expr, tokens = parse_unary tokens in
  let rec helper expr tokens =
    match
      match_token tokens ~f:(function
        | Slash -> Some Expr.Divide
        | Star -> Some Expr.Multiply
        | _ -> None)
    with
    | None -> expr, tokens
    | Some (sym, tokens) ->
      let right, tokens = parse_unary tokens in
      let expr = Expr.Binary (Infix (expr, sym, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_unary tokens : Expr.expression * Token.t list =
  match
    match_token tokens ~f:(function
      | Bang -> Some (fun expr -> Expr.Not expr)
      | Minus -> Some (fun expr -> Expr.Negative expr)
      | _ -> None)
  with
  | None -> parse_primary tokens
  | Some (sym, tokens) ->
    let right, tokens = parse_unary tokens in
    let expr = Expr.Unary (sym right) in
    expr, tokens

and parse_primary tokens =
  match
    match_token tokens ~f:(function
      | False -> Some (Expr.Bool false)
      | True -> Some (Expr.Bool true)
      | Nil -> Some Expr.Nil
      | Identifier s | String s -> Some (Expr.String s)
      | Number f -> Some (Expr.Number f)
      | _ -> None)
  with
  | Some (literal, tokens) -> Expr.Literal literal, tokens
  | None ->
    (match
       match_token tokens ~f:(function
         | Left_paren -> Some ()
         | _ -> None)
     with
     | None ->
       print_token_err ?token:(List.hd tokens) "Expect expression";
       assert false
     | Some ((), tokens) ->
       let expr, tokens = parse_expression tokens in
       (match
          match_token tokens ~f:(function
            | Right_paren -> Some ()
            | _ -> None)
        with
        | None ->
          print_token_err ?token:(List.hd tokens) "Expect ')' after expression";
          assert false
        | Some ((), tokens) -> expr, tokens))
;;
