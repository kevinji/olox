open! Core
open! Async
open! Import

let print_token_err (token : Token.t) message =
  let where =
    match token.type_ with
    | Eof -> " at end"
    | _ -> sprintf " at '%s'" token.lexeme
  in
  print_err ~line:token.line ~where message
;;

module Binary_match = struct
  type 'a t =
    | No_match of Token.t list
    | Match of 'a * Token.t list

  let return tokens = No_match tokens

  let bind t ~f =
    match t with
    | Match _ | No_match [] -> t
    | No_match (token :: tokens) ->
      (match f token.type_ with
       | None -> t
       | Some expr -> Match (expr, tokens))
  ;;

  let to_option = function
    | No_match _ -> None
    | Match (expr, tokens) -> Some (expr, tokens)
  ;;
end

let operator ~token_type ~(sym : Expr.operator) (tokens : Token.t list) =
  match tokens with
  | token :: tokens when Token.Type.equal token.type_ token_type -> Some (sym, tokens)
  | _ -> None
;;

let unary ~token_type ~(sym : Expr.expression -> Expr.unary) (tokens : Token.t list) =
  match tokens with
  | token :: tokens when Token.Type.equal token.type_ token_type -> Some (sym, tokens)
  | _ -> None
;;

let primary ~token_type ~(sym : Expr.literal) (tokens : Token.t list) =
  match tokens with
  | token :: tokens when Token.Type.equal token.type_ token_type -> Some (sym, tokens)
  | _ -> None
;;

let rec any op_list tokens =
  match op_list with
  | [] -> None
  | op :: op_list ->
    (match op tokens with
     | Some _ as result -> result
     | None -> any op_list tokens)
;;

let rec parse_expression tokens = parse_equality tokens

and parse_equality tokens =
  let expr, tokens = parse_comparison tokens in
  let rec helper expr tokens =
    let some_match =
      Binary_match.return tokens
      |> Binary_match.bind ~f:(function
        | Equal -> Some Expr.Equal
        | Bang_equal -> Some Expr.Not_equal
        | _ -> None)
      |> Binary_match.to_option
    in
    match some_match with
    | None -> expr, tokens
    | Some (op, tokens) ->
      let right, tokens = parse_comparison tokens in
      let expr = Expr.Binary (Infix (expr, op, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_comparison tokens : Expr.expression * Token.t list =
  let expr, tokens = parse_addition tokens in
  let some_match =
    Binary_match.return tokens
    |> Binary_match.bind ~f:(function
      | Greater -> Some Expr.Greater
      | Greater_equal -> Some Expr.Greater_equal
      | Less -> Some Expr.Less
      | Less_equal -> Some Expr.Less_equal
      | _ -> None)
    |> Binary_match.to_option
  in
  match some_match with
  | None -> expr, tokens
  | Some (op, tokens) ->
    let right, tokens = parse_addition tokens in
    Expr.Binary (Infix (expr, op, right)), tokens

and parse_addition tokens : Expr.expression * Token.t list =
  let expr, tokens = parse_multiplication tokens in
  let rec helper expr tokens =
    let some_match =
      Binary_match.return tokens
      |> Binary_match.bind ~f:(function
        | Minus -> Some Expr.Minus
        | Plus -> Some Expr.Plus
        | _ -> None)
      |> Binary_match.to_option
    in
    match some_match with
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
    let some_match =
      any
        [ operator ~token_type:Slash ~sym:Divide
        ; operator ~token_type:Star ~sym:Multiply
        ]
        tokens
    in
    match some_match with
    | None -> expr, tokens
    | Some (sym, tokens) ->
      let right, tokens = parse_unary tokens in
      let expr = Expr.Binary (Infix (expr, sym, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_unary tokens : Expr.expression * Token.t list =
  let some_match =
    any
      [ unary ~token_type:Bang ~sym:(fun expr -> Not expr)
      ; unary ~token_type:Minus ~sym:(fun expr -> Negative expr)
      ]
      tokens
  in
  match some_match with
  | None -> parse_primary tokens
  | Some (sym, tokens) ->
    let right, tokens = parse_unary tokens in
    let expr = Expr.Unary (sym right) in
    expr, tokens

and parse_primary tokens =
  let some_match =
    any
      [ primary ~token_type:False ~sym:(Bool false)
      ; primary ~token_type:True ~sym:(Bool true)
      ; primary ~token_type:Nil ~sym:Nil (* Number and String *)
      ]
      tokens
  in
  match some_match with
  | None ->
    (match tokens with
     | token :: tokens ->
       (match token.type_ with
        | Identifier s | String s -> Expr.Literal (String s), tokens
        | Number f -> Expr.Literal (Number f), tokens
        | Left_paren ->
          let expr, tokens = parse_expression tokens in
          (match tokens with
           | token :: tokens when Token.Type.equal token.type_ Right_paren -> expr, tokens
           | _ ->
             print_token_err token "Expect ')' after expression";
             assert false)
        | _ ->
          print_token_err token "Expect expression";
          assert false)
     | [] -> assert false)
  | Some (literal, tokens) -> Expr.Literal literal, tokens
;;
