
open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)
(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks : (token list * expr) = 
  
  match lookahead toks with
| Some Tok_Let -> let rest = match_token toks Tok_Let in parse_let rest
| Some Tok_If -> let rest = match_token toks Tok_If in parse_IF rest
| Some Tok_Fun -> let rest = match_token toks Tok_Fun in parse_FunExp rest
| _ -> parse_Or toks
  

(* LetExpr -> let Recursion Tok_ID = Expr in Expr
Recursion -> rec | ε *)

and parse_let toks = 
  let (rem1, ans1) = parse_rec toks in
  let (rem2, ans2_id) = parse_primaryExpr rem1 in

  let rest = match_token rem2 Tok_Equal in
  let (rem3, ans2) = parse_expr rest in

  let rest2 = match_token rem3 Tok_In in
  let (rem4, ans3) = parse_expr rest2 in

  let id = 
    if (match ans2_id with | ID(x) -> true | _ -> false) 
      then match ans2_id with | ID(x) -> x | _ -> failwith "unreachable"
      else raise (InvalidInputException("Was expecting Tok id"))
  in
  (rem4, Let(id, ans1, ans2, ans3))


and parse_rec toks = 
  match lookahead toks with
  | Some Tok_Rec -> (match_token toks Tok_Rec,true)
  | _ -> (toks, false)


(*FunctionExpr -> fun Tok_ID -> Expr *)
and parse_FunExp toks = 
 let peek = lookahead toks in 
 match peek with
| Some Tok_ID peek -> 
  ( 
    let rest = match_token toks (Tok_ID peek) in 
     match lookahead rest with
  | Some Tok_Arrow -> let rest2 = match_token rest Tok_Arrow in 
  let (rem,ans) = parse_expr rest2 in 
  (rem, Fun (peek, ans))
  | _ -> raise (InvalidInputException "parse_funEXP")
  )
| _ -> raise (InvalidInputException "was expecting a tok id")


(*IfExpr -> if Expr then Expr else Expr *)

and parse_IF toks =
  let (rem, ans1) = parse_expr toks in 
  match lookahead rem with
  | Some Tok_Then -> 
    (*GROUP!!! *)
    begin
      let rest = match_token rem Tok_Then in 
      let (rem2, ans2) = parse_expr rest in 
      match lookahead rem2 with
      | Some Tok_Else -> 
        begin
          let rest2 = match_token rem2 Tok_Else in 
          let (rem3, ans3) = parse_expr rest2 in 
          (rem3, If (ans1, ans2, ans3))
        end
      | _ -> raise (InvalidInputException "parse_IF")
    end
  | Some _ -> raise (InvalidInputException "parse_IF")
  | None -> raise (InvalidInputException "parse_IF")

(* OrExpr -> AndExpr || OrExpr | AndExpr *)

and parse_Or toks =
    let (rem, ans1) = parse_And toks in
    match lookahead rem with
    | Some Tok_Or ->
      let rem1 = match_token rem Tok_Or in
      let (rem2, ans2) = parse_Or rem1 in
      (rem2, Binop(Or, ans1, ans2))
    | _ -> (rem, ans1)

    (*AndExpr -> EqualityExpr && AndExpr | EqualityExpr*)

and parse_And toks = 
      let (rest, left) = parse_Eq toks in
      match lookahead rest with
      | Some Tok_And ->
        let rest1 = match_token rest Tok_And in
        let (rest2, right) = parse_And rest1 in
        (rest2, Binop(And, left, right))
      | _ -> (rest, left)

(* EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr *)
and parse_Eq toks = 
  let (rest, ans1) = parse_Rel toks in 
  match lookahead rest with 
  | Some Tok_Equal -> 
    let rest1 = match_token rest Tok_Equal in 
    let (rem, ans2) = parse_Eq rest1 in 
    (rem, Binop(Equal, ans1,ans2))

  | Some Tok_NotEqual -> 
    let rest1 = match_token rest Tok_NotEqual in 
    let (rem, ans2) = parse_Eq rest1 in 
    (rem, Binop(NotEqual, ans1,ans2))
  | _ -> (rest, ans1)

(*RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
RelationalOperator -> < | > | <= | >= *)

and parse_Rel toks =
    let (rest, ans1) = parse_Add toks in
    match lookahead rest with
    | Some Tok_Less ->
        let rest1 = match_token rest Tok_Less in
        let (rem, ans2) = parse_Rel rest1 in
        (rem, Binop(Less, ans1, ans2))
    | Some Tok_LessEqual ->
        let rest1 = match_token rest Tok_LessEqual in
        let (rem, ans2) = parse_Rel rest1 in
        (rem, Binop(LessEqual, ans1, ans2))
    | Some Tok_Greater ->
        let rest1 = match_token rest Tok_Greater in
        let (rem, ans2) = parse_Rel rest1 in
        (rem, Binop(Greater, ans1, ans2))
    | Some Tok_GreaterEqual ->
        let rest1 = match_token rest Tok_GreaterEqual in
        let (rem, ans2) = parse_Rel rest1 in
        (rem, Binop(GreaterEqual, ans1, ans2))
    | _ -> (rest, ans1)


(*AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
AdditiveOperator -> + | - *)

and  parse_Add toks =
  let (rem, ans1) = parse_Mult toks in
  match lookahead rem with
  | Some Tok_Add ->
    let rem1 = match_token rem Tok_Add in
    let (rem2, ans2) = parse_Add rem1 in
    (rem2, Binop( Add, ans1, ans2))

  | Some Tok_Sub ->
    let rem1 = match_token rem Tok_Sub in
    let (rem2, ans2) = parse_Add rem1 in
    (rem2, Binop( Sub, ans1, ans2))
  | _ -> (rem, ans1)

  (*MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
MultiplicativeOperator -> * | / *)
and  parse_Mult toks =
  let (rest, ans1) = parse_Concat toks in 
  match lookahead rest with 
  | Some Tok_Mult -> 
    let tok1 = match_token rest Tok_Mult in 
    let (rem, ans2) = parse_Mult tok1 in 
    (rem, Binop (Mult, ans1, ans2))
  | Some Tok_Div -> 
    let tok1 = match_token rest Tok_Div in 
    let (rem, ans2) = parse_Mult tok1 in 
    (rem, Binop (Div, ans1, ans2))
  | _ -> (rest, ans1)

  (*ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr *)

and  parse_Concat toks =
  let (rem, expr1) = parse_UnaryExpr toks in
  match lookahead rem with
  | Some Tok_Concat ->
      let toks2 = match_token rem Tok_Concat in
      let (toks3, expr2) = parse_Concat toks2 in
      (toks3, Binop (Concat, expr1, expr2))
  | _ -> (rem, expr1)



  (*UnaryExpr -> not UnaryExpr | FunctionCallExpr  *)

and  parse_UnaryExpr toks =
match lookahead toks with
| Some Tok_Not ->
    let rest = match_token toks Tok_Not in
    let (rem, ans) = parse_UnaryExpr rest in
    (rem, Not(ans))
| _ ->  
  let (rema, answ) = (parse_FunctionCallExpr toks) in
  (rema, answ)

(*FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr *)

and parse_FunctionCallExpr toks =
  let (rest, sol) = parse_primaryExpr toks in
  match lookahead rest with
  | Some Tok_Int x -> 
    let (rem, ans) = parse_primaryExpr rest in
    (rem, FunctionCall (sol, ans))
  | Some Tok_Bool x -> 
    let (rem, ans) = parse_primaryExpr rest in
    (rem, FunctionCall (sol, ans))
  | Some Tok_String x -> 
    let (rem, ans) = parse_primaryExpr rest in
    (rem, FunctionCall (sol, ans))
  | Some Tok_ID x -> 
    let (rem, ans) = parse_primaryExpr rest in
    (rem, FunctionCall (sol, ans))
  | Some Tok_LParen -> 
    let (rem, ans) = parse_primaryExpr rest in
    (rem, FunctionCall (sol, ans))
  | _ ->  (rest, sol)
  
  
 
  (*PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)

and parse_primaryExpr toks = 
  let peek = lookahead toks in
  match peek with
  | Some Tok_Int x -> 
    let rest = match_token toks (Tok_Int x) in
    (rest, Value(Int x))

  | Some Tok_Bool x -> 
    let rest = match_token toks (Tok_Bool x) in
    (rest, Value(Bool x))

  | Some Tok_String x -> 
    let rest = match_token toks (Tok_String x) in
    (rest, Value(String x))
    
  | Some Tok_ID x -> 
    let rest = match_token toks (Tok_ID x) in
    (rest, ID x)

  | Some Tok_LParen -> 
    let rest = match_token toks Tok_LParen in
    let (rest2, ans) = parse_expr rest in
    let rem = match_token rest2 Tok_RParen in
    (rem, ans)
  | _ -> raise (InvalidInputException "parse primary expr failed")

let rec parse_Mutop toks = 
  
  match lookahead toks with
  | Some Tok_Def -> parse_DefMutop toks
  | Some Tok_DoubleSemi -> ([], NoOp)
  | _-> parse_ExprMutop toks
  
  (*DefMutop -> def Tok_ID = Expr ;; *)
  and parse_DefMutop toks = 
  let rest = match_token toks Tok_Def in 
  match lookahead rest with 
  | Some (Tok_ID x) -> 
    let rest1 = match_token rest (Tok_ID x) in 
    let rest2 = match_token rest1 Tok_Equal in 
    let (rem1, sol) = parse_expr rest2 in 
    let rest3 = match_token rem1 Tok_DoubleSemi in 
    (rest3, Def(x, sol))
  | _ -> raise (InvalidInputException "parse_DefMutop input prob")
  
  and parse_ExprMutop toks =
  let (rest, sol) = parse_expr toks in
  match lookahead rest with
  | Some Tok_DoubleSemi -> (rest, Expr sol)
  | _ -> raise (InvalidInputException "parse Expr error")
  
  and parse_mutop toks =
    let (rem, sol) = parse_Mutop toks in
    ([], sol)