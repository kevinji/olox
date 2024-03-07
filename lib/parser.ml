open! Core
open! Async
open! Import

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

let operator ~token_type ~(op : (_, _) Expr.operator) (tokens : Token.t list) =
  match tokens with
  | token :: tokens when Token.Type.equal token.type_ token_type -> Some (op, tokens)
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
        | EQUAL -> Some Expr.Equal
        | BANG_EQUAL -> Some Expr.Not_equal
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

and parse_comparison tokens : bool Expr.expression * Token.t list =
  let expr, tokens = parse_addition tokens in
  let some_match =
    Binary_match.return tokens
    |> Binary_match.bind ~f:(function
      | GREATER -> Some Expr.Greater
      | GREATER_EQUAL -> Some Expr.Greater_equal
      | LESS -> Some Expr.Less
      | LESS_EQUAL -> Some Expr.Less_equal
      | _ -> None)
    |> Binary_match.to_option
  in
  match some_match with
  | None -> expr, tokens
  | Some (op, tokens) ->
    let right, tokens = parse_addition tokens in
    Expr.Binary (Infix (expr, op, right)), tokens

and parse_addition tokens : float Expr.expression * Token.t list =
  let expr, tokens = parse_multiplication tokens in
  let rec helper expr tokens =
    let some_match =
      Binary_match.return tokens
      |> Binary_match.bind ~f:(function
        | MINUS -> Some Expr.Minus
        | PLUS -> Some Expr.Plus
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

and parse_multiplication tokens : float Expr.expression * Token.t list =
  let expr, tokens = parse_unary tokens in
  let rec helper expr tokens =
    let some_match =
      any
        [ operator ~token_type:SLASH ~op:Divide; operator ~token_type:STAR ~op:Multiply ]
        tokens
    in
    match some_match with
    | None -> expr, tokens
    | Some (op, tokens) ->
      let right, tokens = parse_unary tokens in
      let expr = Expr.Binary (Infix (expr, op, right)) in
      helper expr tokens
  in
  helper expr tokens

and parse_unary tokens =
  let some_match =
    any [ operator ~token_type:BANG ~op:Not; operator ~token_type:MINUS ~op:Negative ]
  in
  match some_match with
  | None -> parse_primary tokens
  | Some (op, tokens) ->
    let right = parse_unary tokens in
    Expr.Unary (op right)

and parse_primary tokens =
  let some_match =
    any
      [ operator ~token_type:FALSE ~op:(Bool false)
      ; operator ~token_type:TRUE ~op:(Bool true)
      ; operator ~token_type:NIL ~op:Nil (* Number and String *)
      ]
  in
  match some_match with
  | None -> assert false
  | Some (literal, tokens) -> Expr.Literal literal, tokens
;;
